-- Copyright © 2010 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the GNU LGPL version 2.

module Database
where

import Codec.MIME.String.Parse
import Codec.MIME.String.Types
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory
import qualified System.IO.Strict as Str
import System.FilePath

newtype Database a = Database { openDatabase :: StateT (M.Map String (S.Set FilePath)) IO a }

instance Monad Database where
  return             = Database . return
  (Database m) >>= f = Database $ m >>= openDatabase . f

runDatabase :: Database a -> IO a
runDatabase (Database m) = evalStateT m M.empty

getSeq :: String -> Database (S.Set FilePath)
getSeq s = Database $ do
  cache <- gets $ M.lookup s
  case cache of
    Just l -> return l
    Nothing -> do
      f <- lift $ catch (Str.readFile $ ".seq" </> s) (const $ return "")
      let ls = S.fromList $ lines f
      modify $ M.insert s ls
      return ls

putSeq :: String -> S.Set FilePath -> Database ()
putSeq s ls = Database $ do
  lift $ createDirectoryIfMissing True $ takeDirectory $ ".seq" </> s
  lift $ writeFile (".seq" </> s) $ unlines $ S.elems ls
  modify $ M.insert s ls

allSeq :: Database (S.Set FilePath)
allSeq = Database $ lift $ recurseInto "."
  where

    recurseInto :: FilePath -> IO (S.Set FilePath)
    recurseInto d = do
      es <- getDirectoryContents d
      sets <- forM es $ \e -> case e of
        "cur" -> addMessages $ d </> e
        "new" -> addMessages $ d </> e
        "tmp" -> return S.empty
        '.':_ -> return S.empty
        _     -> recurseInto $ d </> e
      return $ foldr S.union S.empty sets

    addMessages :: FilePath -> IO (S.Set FilePath)
    addMessages d = do
      ms <- getDirectoryContents d
      return $ S.fromList $ map (d </>) $ filter (\e -> case e of '.':_ -> False; _ -> True) ms

getMsg :: FilePath -> Database Message
getMsg m = Database $ do
  f <- lift $ Str.readFile m
  return $ parse f
