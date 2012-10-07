{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.Git
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Filesystem (isDirectory, removeTree)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           GHC.Conc
import           Prelude.General
import           Shelly
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

withRepository :: Text -> (Repository -> IO ()) -> IO ()
withRepository n f = do let p = fromText n
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

  withRepository "test.git" $ \repo -> do
    file <- BL.readFile (head (files opts))

    let revs = readSvnDump file
    (c, lastRev) <-
      foldM (\(co, _) rev -> do
                when (isJust co) $
                  reportProgress False (revNumber rev) (fromJust co)
                co' <- applyRevision repo co rev
                return (co', rev))
            (Nothing, head revs) revs

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
    showProgress num _ = do
      putStr $ "Converting " ++ show num ++ "...\r"
      -- void (forkOS (update_ obj))

applyRevision :: Repository -> Maybe Commit -> Revision
              -> IO (Maybe Commit)
applyRevision repo c rev = do
  case foldl' (\co op -> co `seq` foldOp co op)
              (Left (makeCommit c)) (revOperations rev) of
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

    foldOp :: Either Commit (IO Commit) -> Operation
           -> Either Commit (IO Commit)
    foldOp c' op =
      case opAction op of
        Add ->
          case opKind op of
            Directory -> c'
            File      -> addFile op c'

        Change  -> c'
        Replace -> c'
        Delete  -> c'

    addFile op c'
      | BL.null (opContents op) = c'
      | otherwise =
        Right (case c' of Left f -> return f; Right x -> x
               >>= updateCommit (opFilePath op) (wrapBlob op repo)
               >>= update)

opFilePath :: Operation -> FilePath
opFilePath = fromText . T.pack . opPathname

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

-- Main.hs (hsubconvert) ends here
