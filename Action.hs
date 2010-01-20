-- Copyright © 2010 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the GNU LGPL version 2.

module Action
where

import Codec.MIME.String
import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import IO
import System.FilePath
import System.Locale
import System.Posix.Files
import System.Process
import Text.Printf
import Text.Regex.Posix

import Database

header :: String -> Message -> [String]
header h = map (dropWhile (== ' ') . h_body) . filter ((== (map toLower h ++ ":")) . map toLower . h_raw_name) . mi_headers . m_message_info

m_header :: String -> Message -> Maybe String
m_header h = listToMaybe . header h

m_date :: Message -> Maybe UTCTime
m_date m = (m_header "date" m >>= parseTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z") `mplus` (m_header "date" m >>= parseTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z (%Z)")

data Query
  = InSeq String
  | WithHeader String String
  | Not Query
  | Or [Query]
  | And [Query]
  deriving (Show)

type QueryM = StateT (S.Set FilePath) Database

buildQuery :: Query -> QueryM ()
buildQuery (InSeq s) = do
  ms <- lift $ getSeq s
  modify $ S.intersection ms
buildQuery (WithHeader h r) = do
  let cr = makeRegexOpts compIgnoreCase execBlank r
  ms <- get
  forM_ (S.elems ms) $ \m -> do
    mime <- lift $ getMsg m
    let hs = header h mime
    if any (match cr) hs
      then return ()
      else modify $ S.delete m
buildQuery (Not q) = do
  ms <- get
  ms' <- lift $ execStateT (buildQuery q) ms
  put $ S.difference ms ms'
buildQuery (Or qs) = do
  ms <- get
  ms' <- forM qs $ \q -> lift $ execStateT (buildQuery q) ms
  put $ foldr S.union S.empty ms'
buildQuery (And qs) = foldr (>>) (return ()) $ map buildQuery qs

runQuery :: [Query] -> Database (S.Set FilePath)
runQuery qs = do
  all <- allSeq
  execStateT (foldr (>>) (return ()) $ map buildQuery qs) all

data Action
  = List
  | Show
  | SetSeq String
  | AddToSeq String
  | RemoveFromSeq String
  deriving (Show)

sortMessages :: [Message] -> [Message]
sortMessages = sortBy (compare `on` m_date)

listMessage :: Message -> IO ()
listMessage m = do
  tz <- getCurrentTimeZone
  let date = maybe "                 " (formatTime defaultTimeLocale "%x %X" . utcToLocalTime tz) $ m_date m
  let from = maybe "Unknown Sender" id $ m_header "from" m
  let subj = maybe "Unknown Subject" id $ m_header "subject" m
  let list = maybe "Personal" id $ m_header "list-id" m
  printf "%16.16s %s %30.30s %s\n" list date from subj

runAction :: Action -> [Query] -> IO ()
runAction List qs = do
  ms <- runDatabase $ do
    fs <- runQuery qs
    putSeq "cur" fs
    forM (S.elems fs) $ getMsg
  forM_ (sortMessages ms) listMessage
runAction Show qs = do
  ms <- runDatabase $ do
    fs <- runQuery qs
    old <- getSeq "seen"
    putSeq "cur" fs
    putSeq "seen" $ S.union old fs
    forM (S.elems fs) $ getMsg
  forM_ (sortMessages ms) $ \m -> do
    listMessage m
    getLine
    showMessageContent $ m_message_content m
runAction (SetSeq s) qs = runDatabase $ do
  fs <- runQuery qs
  putSeq "cur" fs
  putSeq s fs
runAction (AddToSeq s) qs = runDatabase $ do
  fs <- runQuery qs
  old <- getSeq s
  putSeq "cur" fs
  putSeq s $ S.union old fs
runAction (RemoveFromSeq s) qs = runDatabase $ do
  fs <- runQuery qs
  old <- getSeq s
  putSeq "cur" fs
  putSeq s $ S.difference old fs

run :: String -> [String] -> String -> IO ()
run cmd args send = do
  (Just stdin, Nothing, Nothing, ph) <- createProcess $ CreateProcess
    { cmdspec = RawCommand cmd args
    , cwd = Nothing
    , env = Nothing
    , std_in = CreatePipe
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = False
    }
  hPutStr stdin send
  hClose stdin
  waitForProcess ph
  return ()

showMessageContent :: MessageContent -> IO ()
showMessageContent (NoContent (ContentType t1 t2 _)) = putStrLn $ "Empty part with type " ++ t1 ++ "/" ++ t2
showMessageContent (Body (ContentType t1 t2 _) _ d)  = run "run-mailcap" ["--action=view", t1 ++ "/" ++ t2 ++ ":-"] d
showMessageContent (Data (Just e) (ContentType t1 t2 _) _ d) = run "run-maincap" ["--action=view", t1 ++ "/" ++ t2 ++ ":" ++ show e ++ ":-"] d
showMessageContent (Data Nothing (ContentType t1 t2 _) _ d) = run "run-mailcap" ["--action=view", t1 ++ "/" ++ t2 ++ ":-"] d
showMessageContent (Mixed (Multipart _ ms _)) = mapM_ showMessageContent $ map m_message_content ms
showMessageContent (Alternative (Multipart _ ms _)) = mapM_ showMessageContent $ map m_message_content ms
showMessageContent (Parallel (Multipart _ ms _)) = mapM_ showMessageContent $ map m_message_content ms
showMessageContent (Digest (Multipart _ ms _)) = mapM_ showMessageContent $ map m_message_content ms
showMessageContent (RFC822 _ _ m) = showMessageContent $ m_message_content m
