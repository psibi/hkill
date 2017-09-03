{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Linux.Process where

import Data.Monoid ((<>))
import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import Data.Text (Text, unpack, pack)
import Data.Char (isDigit)
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (isDirectory, getFileStatus)
import System.Directory (listDirectory)
import Control.Monad (filterM)
import System.Posix.Signals

newtype Pid = Pid
  { pid :: Int
  } deriving (Show, Read, Eq, Ord)

data ProcessInfo = ProcessInfo
  { procName :: Text
  , procState :: Text
  , procPid :: Pid
  , procPpid :: Pid
  , procVmSize :: Text
  , procThreads :: Int
  } deriving (Show, Eq, Ord)

procFs :: String
procFs = "/proc"

processInfo :: Pid -> String
processInfo id = "/proc/" <> (show $ pid id) <> "/status"

skipLines :: Text -> Parser Text
skipLines txt = do
  string txt <|> (takeTill isEndOfLine *> endOfLine *> skipLines txt)

skipLines_ :: Text -> Parser ()
skipLines_ txt = skipLines txt >> return ()

pidParser :: Text -> Parser Pid
pidParser txt = do
  return $ Pid $ read $ unpack txt

intParser :: Text -> Parser Int
intParser txt = return $ read $ unpack txt

parseProcessInfo :: Parser ProcessInfo
parseProcessInfo = do
  procName <- string "Name:" *> skipWhile (isHorizontalSpace) *> takeTill isEndOfLine
  endOfLine
  procState <- string "State:" *> skipWhile isHorizontalSpace *> takeTill isEndOfLine
  endOfLine
  skipLines_ "Pid:"
  procPid <- skipWhile isHorizontalSpace *> takeTill isEndOfLine >>= pidParser
  endOfLine
  skipLines_ "PPid:"
  procPpid <- skipWhile isHorizontalSpace *> takeTill isEndOfLine >>= pidParser
  endOfLine
  skipLines_ "VmSize:"
  procVmSize <- skipWhile isHorizontalSpace *> takeTill isEndOfLine
  endOfLine
  skipLines_ "Threads:"
  procThreads <- skipWhile isHorizontalSpace *> takeTill isEndOfLine >>= intParser
  return $
    ProcessInfo
    { ..
    }

parsePid :: Parser Pid
parsePid = do
  takeTill (== '(')
  char '('
  pid <- decimal
  char ')'
  return $ Pid pid

selectedPid :: Text -> Either String Pid
selectedPid = parseOnly parsePid

getProcessInfo :: Pid -> IO String
getProcessInfo pid = do
  !contents <- readFile (processInfo pid)
  return contents

isNumber :: String -> Bool
isNumber [] = False
isNumber xs = Prelude.all isDigit xs

isPidFile :: FilePath -> IO Bool
isPidFile fp = do
  fstatus <- getFileStatus $ ("/proc" </> fp)
  return $ isDirectory fstatus && (isNumber fp)

getPids :: IO [Pid]
getPids = do
  files <- listDirectory procFs
  (files' :: [FilePath]) <- filterM isPidFile files
  return $ map (Pid . read) files'

getAllProcessInfo :: IO [String]
getAllProcessInfo = do
  pids <- getPids
  mapM getProcessInfo pids

getAllProcessInfoDS :: IO [Either String ProcessInfo]
getAllProcessInfoDS = do
  pinfos <- getAllProcessInfo
  return $ map (\p -> parseOnly parseProcessInfo (pack p)) pinfos

samePid :: Pid -> Either String ProcessInfo -> Bool
samePid _ (Left _) = False
samePid pid (Right pinfo) = (procPid pinfo) == pid

filterPid :: Pid -> [Either String ProcessInfo] -> [Either String ProcessInfo]
filterPid pid = filter (samePid pid)

softKill :: Pid -> IO ()
softKill (Pid pid) = signalProcess softwareTermination (fromIntegral pid)

hardKill :: Pid -> IO ()
hardKill (Pid pid) = signalProcess killProcess (fromIntegral pid)
