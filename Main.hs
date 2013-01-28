{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Control.Concurrent.ParallelIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.Git
import           Data.Stringable hiding (fromText, length)
import           Data.Text.Format ( format )
import           Data.Text.Lazy (Text)
import           Filesystem (isDirectory, removeTree)
import           Filesystem.Path.CurrentOS hiding (null, toText)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           GHC.Conc
import           Options.Applicative
import           Prelude.General
import           Subversion.Dump
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

hSubconvert :: Parser HSubconvert
hSubconvert = HSubconvert
    <$> option ( long "jobs" <> short 'j' <>
                 help "Run INT concurrent jobs (default: 4)" )
    <*> switch ( long "verbose" <> short 'v' <>
                 help "Report progress verbosely" )
    <*> switch ( long "debug" <> short 'D' <>
                 help "Report debug information" )
    <*> arguments
    <*> header hSubconvertSummary
    <*> progDesc "Faithful conversion of Subversion repositories to Git"

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
    (Just c, _) <-               -- _ = finalState
      flip runStateT (HSubconvertState []) $
        foldM (\co rev -> do
                reportProgress False (revNumber rev)

                co' <- applyRevision repo co rev
                -- Store a reference to the commit tree's hash rather than the
                -- object itself, since in most cases it will never be
                -- referenced, and even when it is it should only happen a few
                -- times.  It's much cheaper heap-wise to just reload the Git
                -- object from disk at that time, since even then only a
                -- sub-tree will typically be needed
                let ObjRef ct = commitTree co'
                ctr <- lift $ objectRef ct
                pastTrees %= (:) (revNumber rev, ctr)
                return (Just co'))
              Nothing revs

    cid <- objectId c
    writeRef_ $ createRef "refs/heads/master" (RefTargetId cid) repo
    writeRef_ $
      createRef "HEAD" (RefTargetSymbolic "refs/heads/master") repo

    putStr "\nConversion completed\n"

    stopGlobalPool

  where
    reportProgress force num
      | force || num < 10 = showProgress num
      | num < 100        = when (num `mod` 10 == 0)   $ showProgress num
      | num < 1000       = when (num `mod` 100 == 0)  $ showProgress num
      | otherwise        = when (num `mod` 1000 == 0) $ showProgress num

    showProgress num =
      lift $ putStr $ "Converting " ++ show num ++ "...\r"

applyRevision :: Repository -> Maybe Commit -> Revision -> SIO Commit
applyRevision repo maybeCommit rev = do
  lift $ debugL (format "Applying revision r{}" [ revNumber rev ])
  foldl' foldOp (return (makeCommit maybeCommit)) (revOperations rev)
  >>= lift . update

  where
    makeCommit co =
      let nco = case co of
                  Nothing -> createCommit repo
                  Just p  -> (createCommit repo) {
                      commitTree     = commitTree p
                    , commitParents = [ObjRef p] }
          sig = Signature {
                    signatureName  = fromMaybe "Unknown" (revAuthor rev)
                  , signatureEmail = "unknown@unknown.org"
                  , signatureWhen  = revDate rev }

      in nco { commitAuthor    = sig
             , commitCommitter = sig
             , commitLog       = fromMaybe "" (revComment rev) }

    foldOp :: SIO Commit -> Operation -> SIO Commit
    foldOp c op
      | opKind op == File && BL.null (opContents op) = c

      | opKind op == File
        && (opAction op == Add || opAction op == Change) = do
        lift $ debugL (format "F{}: {}"
                              [ if opAction op == Add then "A" else "C"
                              , opPathname op ])
        addFile c op

      | opAction op == Delete = do
        lift $ debugL (format "?D: {}" [ opPathname op ])
        deleteEntry c op

      | isJust (opCopyFromRev op)
        && opKind op == Directory && opAction op == Add = do
        lift $ debugL (format "DA: {} [r{}] -> {}"
                              [ fromJust (opCopyFromPath op)
                              , show $ fromJust (opCopyFromRev op)
                              , opPathname op ])
        copyEntry c op

      | otherwise = c

    addFile c op
      | isJust (opCopyFromRev op) = copyEntry c op
      | otherwise = updateEntry c op (wrapBlob op repo)

    updateEntry :: SIO Commit -> Operation -> TreeEntry -> SIO Commit
    updateEntry c op entry =
      c >>= lift . updateCommit (opFilePath op) entry

    deleteEntry :: SIO Commit -> Operation -> SIO Commit
    deleteEntry c op =
      c >>= lift . removeFromCommitTree (opFilePath op)

    copyEntry :: SIO Commit -> Operation -> SIO Commit
    copyEntry c op = do
      pastTree <- getPastTree (fromJust (opCopyFromRev op))
      case pastTree of
        Nothing -> do
          lift $ errorL $ format "Could not find past tree for r{}"
                                 [ fromJust (opCopyFromRev op) ]
          c
        Just pastTree' -> do
          c' <- c
          -- Lookup the past object to be copied from the pastTree'.  c' is
          -- merely the "reference" object, mainly so withObject knows where
          -- the repository is.
          entry <- lift $ withObject pastTree' c' $ \pt -> do
            pastEntry <- lookupTreeEntry (opCopyFromFilePath op) pt
            case pastEntry of
              Nothing ->
                error $ toString
                  $ format "Could not find {} in tree r{}"
                           [ fromJust (opCopyFromPath op)
                           , show $ fromJust (opCopyFromRev op) ]
              Just entry -> return entry
          updateEntry c op entry

getPastTree :: Int -> SIO (Maybe (ObjRef Tree))
getPastTree rev = return . lookup rev . (^.pastTrees) =<< get

opFilePath :: Operation -> FilePath
opFilePath = toFilePath . opPathname

opCopyFromFilePath :: Operation -> FilePath
opCopyFromFilePath = toFilePath . fromJust . opCopyFromPath

opBlob :: Operation -> Repository -> Blob
opBlob = createBlob . lazyToStrict . opContents

wrapBlob :: Operation -> Repository -> TreeEntry
wrapBlob op repo = BlobEntry (ObjRef (opBlob op repo)) False

lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict lb = BI.unsafeCreate len (go lb)
  where
    len = BLI.foldlChunks (\l sb -> l + B.length sb) 0 lb

    go BLI.Empty _ = return ()
    go (BLI.Chunk (BI.PS fp s l) r) ptr =
      withForeignPtr fp $ \p -> do
        BI.memcpy ptr (p `plusPtr` s) (fromIntegral l)
        go r (ptr `plusPtr` l)

debugL :: Text -> IO ()
debugL = debugM "hsubconvert" . toString

infoL :: Text -> IO ()
infoL = infoM "hsubconvert" . toString

noticeL :: Text -> IO ()
noticeL = noticeM "hsubconvert" . toString

warningL :: Text -> IO ()
warningL = warningM "hsubconvert" . toString

errorL :: Text -> IO ()
errorL = errorM "hsubconvert" . toString

criticalL :: Text -> IO ()
criticalL = criticalM "hsubconvert" . toString

-- Main.hs (hsubconvert) ends here
