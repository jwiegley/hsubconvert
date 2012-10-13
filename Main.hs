{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.Git
import           Data.Stringable hiding (fromText, length)
import           Data.Text.Format ( format )
import           Data.Text.Lazy (Text)
--import qualified Data.Text.Lazy as T
import           Filesystem (isDirectory, removeTree)
import           Filesystem.Path.CurrentOS hiding (null, toText)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           GHC.Conc
import           Prelude.General
import           Subversion.Dump
import           System.Console.CmdArgs
import           System.Environment
import           System.Log.Logger

default (Text)

version :: String
version = "0.0.1"

copyright :: String
copyright = "2012"

hSubconvertSummary :: String
hSubconvertSummary =
  "hsubconvert v" ++ version ++ ", (C) John Wiegley " ++ copyright

data HSubconvert = HSubconvert { jobs    :: Int
                               , verbose :: Bool
                               , debug   :: Bool
                               , files   :: [String] }
    deriving (Data, Typeable, Show, Eq)

hSubconvert :: HSubconvert
hSubconvert = HSubconvert
    { jobs    = def &= name "j" &= typ "INT"
                    &= help "Run INT concurrent jobs (default: 4)"
    , verbose = def &= name "v"
                    &= help "Report progress verbosely"
    , debug   = def &= name "D"
                    &= help "Report debug information"
    , files   = def &= args } &=
    summary hSubconvertSummary &=
    program "hsubconvert" &=
    help "One-time, faithful conversion of Subversion repositories to Git"

data HSubconvertState = HSubconvertState { _pastTrees :: [(Int, ObjRef Tree)] }
                      deriving Show

makeClassy ''HSubconvertState

type SIO = StateT HSubconvertState IO

withRepository :: Text -> (Repository -> IO ()) -> IO ()
withRepository n f = do let p = toFilePath n
                        exists <- isDirectory p
                        when exists $ removeTree p
                        f =<< createRepository p True

main :: IO ()
main = do
  -- process command-line options
  mainArgs <- getArgs
  opts     <- withArgs (if null mainArgs then ["--help"] else mainArgs)
                       (cmdArgs hSubconvert)

  _ <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 4; x -> x

  when (verbose opts) $ updateGlobalLogger "hsubconvert" (setLevel INFO)
  when (debug opts)   $ updateGlobalLogger "hsubconvert" (setLevel DEBUG)

  hSetBuffering stdout NoBuffering

  unless (length (files opts) == 2) $
    void (withArgs ["--help"] (cmdArgs hSubconvert))

  withRepository "test.git" $ \repo -> do
    file <- BL.readFile (head (files opts))

    let revs = readSvnDump file
    (c, _, lastRev) <-           -- _ = finalState
      foldM
        (\(co, st, _) rev -> do
          when (isJust co) $
            reportProgress False (revNumber rev) (fromJust co)
          (co', st') <- flip runStateT st $ applyRevision repo co rev
          let st'' = case co' of
                       Nothing -> st'
                       Just x  ->
                         pastTrees %~ (:) (revNumber rev, x^.commitTree) $ st'
          return (co', st'', rev))
        (Nothing, HSubconvertState [], head revs) revs

    case c of
      Nothing -> putStrLn "No revisions were converted!"
      Just c' -> do
        reportProgress True (revNumber lastRev) c'

        cid <- objectId c'
        writeRef_ $ createRef "refs/heads/master" (RefTargetId cid) repo
        writeRef_ $
          createRef "HEAD" (RefTargetSymbolic "refs/heads/master") repo

        putStr "\nConversion completed\n"

  where
    reportProgress :: Updatable a => Bool -> Int -> a -> IO ()
    reportProgress force num obj
      | force || num < 10 = showProgress num obj
      | num < 100        = when (num `mod` 10 == 0)   $ showProgress num obj
      | num < 1000       = when (num `mod` 100 == 0)  $ showProgress num obj
      | otherwise        = when (num `mod` 1000 == 0) $ showProgress num obj

    showProgress :: Updatable a => Int -> a -> IO ()
    showProgress num _ =
      putStr $ "Converting " ++ show num ++ "...\r"
      -- void (forkOS (update_ obj))

applyRevision :: Repository -> Maybe Commit -> Revision
              -> SIO (Maybe Commit)
applyRevision repo c rev = do
  result <- foldM (\co op -> co `seq` foldOp co op)
                 (Left (makeCommit c)) (revOperations rev)
  case result of
    Left _  -> return c
    Right x -> Just <$> x

  where
    makeCommit co =
      let nco = case co of
                  Nothing -> createCommit repo
                  Just p  ->   commitTree    .~ p^.commitTree
                            $ commitParents .~ [ObjRef p]
                            $ createCommit repo
          sig = Signature {
                    _signatureName  = fromMaybe "Unknown" (revAuthor rev)
                  , _signatureEmail = "unknown@unknown.org"
                  , _signatureWhen  = revDate rev }

      in   commitAuthor    .~ sig
         $ commitCommitter .~ sig
         $ commitLog       .~ fromMaybe "" (revComment rev) $ nco

    foldOp :: Either Commit (SIO Commit) -> Operation
           -> SIO (Either Commit (SIO Commit))
    foldOp c' op
      | opKind op == File && BL.null (opContents op) = return c'

      | opKind op == File && (opAction op == Add || opAction op == Change) = do
        lift $ debugL (format "F{}: {}"
                              [ if opAction op == Add then "A" else "C"
                              , opPathname op ])
        addFile c' op

      | opAction op == Delete = do
        lift $ debugL (format "?D: {}" [ opPathname op ])
        return $ Right $ deleteItem c' op

      | isJust (opCopyFromRev op)
        && opKind op == Directory && opAction op == Add = do
        lift $ debugL (format "DA: {} [r{}] -> {}"
                              [ fromJust (opCopyFromPath op)
                              , show $ fromJust (opCopyFromRev op)
                              , opPathname op ])
        copyEntry c' op

      | otherwise = return c'

    addFile c' op
      | isJust (opCopyFromRev op) = copyEntry c' op
      | otherwise = returnUpdate c' op (wrapBlob op repo)

    copyEntry c' op = do
      pastTree  <- getPastTree (fromJust (opCopyFromRev op))
      case pastTree of
        Nothing -> do
          lift $ errorL $ format "Could not find past tree for r{}"
                                 [ show $ fromJust (opCopyFromRev op) ]
          return c'
        Just pastTree' ->
          applyToCommit c' $ \c'' ->
            lift $ withObject pastTree' c'' $ \pt -> do
              pastEntry <- lookupTreeEntry (opCopyFromFilePath op) pt
              case pastEntry of
                Nothing -> do
                  error $ toString
                    $ format "Could not find {} in tree r{}"
                             [ fromJust (opCopyFromPath op)
                             , show $ fromJust (opCopyFromRev op) ]
                Just entry -> return entry
          >>= returnUpdate c' op

    returnUpdate c' op entry =
      return $ Right $ updateCommit' (opFilePath op) entry c'

    deleteItem c' op =
      applyToCommitAndUpdate c' $ lift . removeFromCommitTree (opFilePath op)

applyToCommit :: Either a (SIO a) -> (a -> SIO b) -> SIO b
applyToCommit eitherCommit f =
  (case eitherCommit of Left x -> return x; Right y -> y) >>= f

applyToCommitAndUpdate :: Updatable a
                       => Either a (SIO a) -> (a -> SIO a) -> SIO a
applyToCommitAndUpdate eitherCommit f =
  applyToCommit eitherCommit f >>= lift . update

updateCommit' :: FilePath -> TreeEntry -> Either Commit (SIO Commit)
              -> SIO Commit
updateCommit' path entry c =
  applyToCommitAndUpdate c $ lift . updateCommit path entry

getPastTree :: Int -> SIO (Maybe (ObjRef Tree))
getPastTree rev = return . lookup rev . (^.pastTrees) =<< get

opFilePath :: Operation -> FilePath
opFilePath = toFilePath . opPathname

opCopyFromFilePath :: Operation -> FilePath
opCopyFromFilePath = toFilePath . fromJust . opCopyFromPath

opBlob :: Operation -> Repository -> Blob
opBlob op = createBlob (lazyToStrict (opContents op))

wrapBlob :: Operation -> Repository -> TreeEntry
wrapBlob op repo = BlobEntry (ObjRef (opBlob op repo)) False

lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict lb = BI.unsafeCreate len $ go lb
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go BLI.Empty _ = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
      withForeignPtr fp $ \p -> do
        BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
        go r (ptr `plusPtr` l)

debugL :: Text -> IO ()
debugL = debugM "pushme" . toString

infoL :: Text -> IO ()
infoL = infoM "pushme" . toString

noticeL :: Text -> IO ()
noticeL = noticeM "pushme" . toString

warningL :: Text -> IO ()
warningL = warningM "pushme" . toString

errorL :: Text -> IO ()
errorL = errorM "pushme" . toString

criticalL :: Text -> IO ()
criticalL = criticalM "pushme" . toString

-- Main.hs (hsubconvert) ends here
