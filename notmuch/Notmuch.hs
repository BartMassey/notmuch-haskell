-- Copyright © 2010 Bart Massey
-- notmuch mail library high-level interface

module Notmuch
where

import NOTMUCH_H

import Control.Monad
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import System.FilePath

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.

-- XXX This should probably be thrown away as hidden
-- internally.
data Status = 
  StatusSuccess |
  StatusOutOfMemory |
  StatusReadOnlyDatabase |
  StatusXapianException |
  StatusFileError |
  StatusFileNotEmail |
  StatusDuplicateMessageId |
  StatusNullPointer |
  StatusTagTooLong |
  StatusUnbalancedFreezeThaw
  deriving Enum

statusToString :: Status -> String
statusToString status =
    unsafePerformIO $ do
      cs <- f_notmuch_status_to_string $ fromIntegral $ fromEnum status
      peekCString cs

type Database = Ptr S__notmuch_database

databaseCreate :: FilePath -> IO Database
databaseCreate name = do
  db <- withCString name f_notmuch_database_create
  when (db == nullPtr) $
       fail "database create failed"
  return db

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
data DatabaseMode = 
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
    deriving Enum

databaseOpen :: FilePath -> DatabaseMode -> IO Database
databaseOpen name databaseMode = do
  db <- withCString name $
        flip f_notmuch_database_open $
        fromIntegral $ fromEnum databaseMode
  when (db == nullPtr) $
       fail "database open failed"
  return db

databaseClose :: Database -> IO ()
databaseClose db = f_notmuch_database_close db

databaseGetPath :: Database -> IO FilePath
databaseGetPath db = do
  cs <- f_notmuch_database_get_path db
  peekCString cs

databaseGetVersion :: Database -> IO Int
databaseGetVersion db = do
  v <- f_notmuch_database_get_version db
  return $ fromIntegral v

resultBool :: CInt -> IO Bool
resultBool 0 = return False
resultBool _ = return True

databaseNeedsUpgrade :: Database -> IO Bool
databaseNeedsUpgrade db =
  f_notmuch_database_needs_upgrade db >>= resultBool

statusCheck :: CInt -> IO ()
statusCheck 0 = return ()
statusCheck s = fail $ statusToString $ toEnum $ fromIntegral s

type UpgradeCallback = String -> Double -> IO ()

databaseUpgrade :: Database -> Maybe UpgradeCallback -> IO ()
databaseUpgrade db (Just callback) = do
  let ccb msg progress = do
        cmsg <- peekCString msg
        let cprogress = realToFrac progress
        callback cmsg cprogress
  cb <- w_notmuch_database_upgrade_1 ccb
  s <- f_notmuch_database_upgrade db cb nullPtr
  statusCheck s
databaseUpgrade db Nothing = do
  s <- f_notmuch_database_upgrade db nullFunPtr nullPtr
  statusCheck s

type Directory = Ptr S__notmuch_directory

databaseGetDirectory :: Database -> FilePath -> IO Directory
databaseGetDirectory db path =
    withCString path $ f_notmuch_database_get_directory db
  
type CMessages = Ptr S__notmuch_messages

type Message = ForeignPtr S__notmuch_message

type Messages = [Message]

-- XXX We provide no way to request a null message pointer,
-- so the message is always returned.  The finalizer will
-- then eventually kill it if it is not needed.

-- XXX This function will fail on dup adds, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseAddMessage :: Database -> FilePath -> IO Message
databaseAddMessage db filename = alloca msgFun where
    msgFun msgPtr = do
      let addMessage fn =
              f_notmuch_database_add_message db fn msgPtr
      s <- withCString filename addMessage
      statusCheck s
      cmsg <- peek msgPtr
      newForeignPtr pf_notmuch_message_destroy cmsg

-- XXX This function will fail on dup remove, rather than
-- succeed.  I have no idea what it should do, and this
-- was easiest.
databaseRemoveMessage :: Database -> FilePath -> IO ()
databaseRemoveMessage db filename = do
  let removeMessage fn = f_notmuch_database_remove_message db fn
  s <- withCString filename removeMessage
  statusCheck s

-- XXX This might want to return a Maybe Message instead
-- of failing if the message is not found.  I don't quite
-- understand the use case yet.
databaseFindMessage :: Database -> String -> IO Message
databaseFindMessage db msgid = do
  let findMessage mid =
          f_notmuch_database_find_message db mid
  cmsg <- withCString msgid findMessage
  when (cmsg == nullPtr) $
       fail "database find message failed"
  newForeignPtr pf_notmuch_message_destroy cmsg
  
iterM :: Monad m => a -> (a -> m Bool) -> (a -> m b) -> m [b]
iterM coln test get = do
  cont <- test coln
  case cont of
    True -> do
      elem <- get coln
      rest <- iterM coln test get
      return $ elem : rest
    False -> return []

iterUnpack :: Ptr a -> (Ptr a -> IO Bool) ->
              (Ptr a -> IO b) -> (Ptr a -> IO ()) ->
              IO [b]
iterUnpack coln f_has_more f_get f_advance =
    iterM coln f_has_more get
    where
      get coln' = do
        e <- f_get coln'
        f_advance coln'
        return e

type CTags = Ptr S__notmuch_tags

type Tags = [String]

unpackTags :: CTags -> IO Tags
unpackTags tags = do
  result <- iterUnpack tags
            (\t -> f_notmuch_tags_has_more t >>= resultBool)
            (\t -> f_notmuch_tags_get t >>= peekCString)
            f_notmuch_tags_advance
  f_notmuch_tags_destroy tags
  return result


databaseGetAllTags :: Database -> IO Tags
databaseGetAllTags db = do
  tags <- f_notmuch_database_get_all_tags db
  when (tags == nullPtr) $
       fail "database get all tags failed"
  unpackTags tags

type Query = ForeignPtr S__notmuch_query

queryCreate :: Database -> String -> IO Query
queryCreate db queryString = do
    query <- withCString queryString $ f_notmuch_query_create db
    when (query == nullPtr) $
         fail "query create failed"
    newForeignPtr pf_notmuch_query_destroy query

-- XXX Deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there.
data SortOrder = 
    SortOldestFirst |
    SortNewestFirst |
    SortMessageID
    deriving Enum

querySetSortOrder :: Query -> SortOrder -> IO ()
querySetSortOrder query sortOrder =
    let setSort query' =
            f_notmuch_query_set_sort query' $
            fromIntegral $ fromEnum sortOrder in
    withForeignPtr query setSort

type Threads = Ptr S__notmuch_threads

type Thread = ForeignPtr S__notmuch_thread

queryThreads :: Query -> IO [Thread]
queryThreads query = withForeignPtr query $ (\q -> do
  threads <- f_notmuch_query_search_threads q
  when (threads == nullPtr) $
       fail "query threads failed"
  result <- iterUnpack threads
            (\t -> f_notmuch_threads_has_more t >>= resultBool)
            (\t -> f_notmuch_threads_get t >>=
                   newForeignPtr pf_notmuch_thread_destroy)
            f_notmuch_threads_advance
  f_notmuch_threads_destroy threads
  return result)

unpackMessages :: CMessages -> IO Messages
unpackMessages messages = do
  result <- iterUnpack messages
            (\t -> f_notmuch_messages_has_more t >>= resultBool)
            (\t -> f_notmuch_messages_get t >>=
                   newForeignPtr pf_notmuch_message_destroy)
            f_notmuch_messages_advance
  f_notmuch_messages_destroy messages
  return result

queryMessages :: Query -> IO Messages
queryMessages query = withForeignPtr query $ (\q -> do
  messages <- f_notmuch_query_search_messages q
  when (messages == nullPtr) $
       fail "query messages failed"
  unpackMessages messages)

queryCountMessages :: Query -> IO Int
queryCountMessages query = withForeignPtr query $
    (\q -> f_notmuch_query_count_messages q >>= return . fromIntegral)

getThreadID :: Thread -> IO String
getThreadID thread = withForeignPtr thread $
    (\t -> f_notmuch_thread_get_thread_id t >>= peekCString)

threadCountMessages :: Thread -> IO Int
threadCountMessages thread = withForeignPtr thread $
    (\t -> f_notmuch_thread_get_total_messages t >>= return . fromIntegral)

threadCountMatchedMessages :: Thread -> IO Int
threadCountMatchedMessages thread = withForeignPtr thread $
    (\t -> f_notmuch_thread_get_matched_messages t >>= return . fromIntegral)

threadGetToplevelMessages :: Thread -> IO Messages
threadGetToplevelMessages thread = withForeignPtr thread $ (\t -> do
  messages <- f_notmuch_thread_get_toplevel_messages t
  when (messages == nullPtr) $
       fail "thread get top-level messages failed"
  unpackMessages messages)

-- XXX This pretty clearly wants to return a list of authors
-- rather than a string containing a comma-separated list of
-- authors, but the underlying interface doesn't provide a
-- way to do that.
threadGetAuthors :: Thread -> IO String
threadGetAuthors thread = withForeignPtr thread $ (\t -> do
  authors <- f_notmuch_thread_get_authors t
  when (authors == nullPtr) $
       fail "thread get authors failed"
  peekCString authors)

threadGetSubject :: Thread -> IO String
threadGetSubject thread = withForeignPtr thread $ (\t -> do
  subject <- f_notmuch_thread_get_subject t
  when (subject == nullPtr) $
       fail "thread get subject failed"
  peekCString subject)

threadGetOldestDate :: Thread -> IO UTCTime
threadGetOldestDate thread = withForeignPtr thread $ (\t -> do
  date <- f_notmuch_thread_get_oldest_date t
  return $ posixSecondsToUTCTime $ realToFrac date)

threadGetNewestDate :: Thread -> IO UTCTime
threadGetNewestDate thread = withForeignPtr thread $ (\t -> do
  date <- f_notmuch_thread_get_newest_date t
  return $ posixSecondsToUTCTime $ realToFrac date)

threadGetTags :: Thread -> IO Tags
threadGetTags thread = withForeignPtr thread $ (\t -> do
  tags <-  f_notmuch_thread_get_tags t
  when (tags == nullPtr) $
       fail "thread get tags failed"
  unpackTags tags)

-- XXX Because of the peculiar way this is implemented and
-- interfaced in notmuch, we provide a Haskell re-implementation
-- instead of trying to use the underlying native function.
messagesCollectTags :: Messages -> IO Tags
messagesCollectTags messages = do
  tagses <- mapM messageGetTags messages
  return $ nub $ concat tagses

messageGetMessageID :: Message -> IO String
messageGetMessageID message = withForeignPtr message $ (\m -> do
  msgid <- f_notmuch_message_get_message_id m
  when (msgid == nullPtr) $
       fail "message get message ID failed"
  peekCString msgid)
  

messageGetThreadID :: Message -> IO String
messageGetThreadID message = withForeignPtr message $ (\m -> do
  tid <- f_notmuch_message_get_thread_id m
  when (tid == nullPtr) $
       fail "message get thread ID failed"
  peekCString tid)
  
messageGetReplies :: Message -> IO Messages
messageGetReplies message = withForeignPtr message $ (\m -> do
  messages <- f_notmuch_message_get_replies m
  when (messages == nullPtr) $
       fail "message get replies failed"
  unpackMessages messages)
  
messageGetFilePath :: Message -> IO FilePath
messageGetFilePath message = withForeignPtr message $ (\m -> do
  path <- f_notmuch_message_get_filename m
  when (path == nullPtr) $
       fail "message get file path failed"
  peekCString path)

data MessageFlag =
    MessageFlagMatch
    deriving Enum             

messageGetFlag :: Message -> MessageFlag -> IO Bool
messageGetFlag message flag =
    let cflag = fromIntegral $ fromEnum flag in
    withForeignPtr message $ (\m ->
      f_notmuch_message_get_flag m cflag >>= resultBool)

messageSetFlag :: Message -> MessageFlag -> Bool -> IO ()
messageSetFlag message flag sense =
    let cflag = fromIntegral $ fromEnum flag
        csense = case sense of True -> 1; False -> 0 in
    withForeignPtr message $ (\m ->
      f_notmuch_message_set_flag m cflag csense)

-- HERE

messageGetTags :: Message -> IO Tags
messageGetTags message = withForeignPtr message $ (\m -> do
  tags <- f_notmuch_message_get_tags m
  when (tags == nullPtr) $
       fail "message get tags failed"
  unpackTags tags)
