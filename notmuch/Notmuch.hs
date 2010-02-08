module Notmuch
where

import NOTMUCH_H

import Control.Monad

-- XXX deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there
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

databaseCreate :: String -> IO Database
databaseCreate name = do
  db <- withCString name f_notmuch_database_create
  when (db == nullPtr) $
       fail "database create failed"
  return db

-- XXX deriving Enum will only work if these fields are in
-- the same order as in notmuch.h and there are no gaps
-- there
data DatabaseMode = 
    DatabaseModeReadOnly |
    DatabaseModeReadWrite
    deriving Enum

databaseOpen :: String -> DatabaseMode -> IO Database
databaseOpen name databaseMode = do
  db <- withCString name $
        flip f_notmuch_database_open $
        fromIntegral $ fromEnum databaseMode
  when (db == nullPtr) $
       fail "database open failed"
  return db

databaseClose :: Database -> IO ()
databaseClose db = f_notmuch_database_close db

databaseGetPath :: Database -> IO String
databaseGetPath db = do
  cs <- f_notmuch_database_get_path db
  peekCString cs

databaseGetVersion :: Database -> IO Int
databaseGetVersion db = do
  v <- f_notmuch_database_get_version db
  return $ fromIntegral v

databaseNeedsUpgrade :: Database -> IO Bool
databaseNeedsUpgrade db = do
  ug <- f_notmuch_database_needs_upgrade db
  case ug of
    0 -> return False
    _ -> return True