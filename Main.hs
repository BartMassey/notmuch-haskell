-- Copyright © 2010 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the GNU LGPL version 2.

module Main
where

import System
import System.Directory
import System.FilePath
import Text.Parsec

import Action
import Parse

main :: IO ()
main = do
  h <- getHomeDirectory
  setCurrentDirectory $ h </> "Mail"
  args <- getArgs
  let p = parse parseCmdLine "" args
  case p of
    Left e -> putStrLn $ "Couldn't parse command line: " ++ show e
    Right (a, q) -> runAction a q
