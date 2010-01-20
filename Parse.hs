-- Copyright © 2010 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the GNU LGPL version 2.

module Parse
where

import Control.Monad
import Text.Parsec
import Text.Parsec.Pos

import Action

get = token id (const $ newPos "" 0 0) return

expect s = token id (const $ newPos "" 0 0) (\s' -> guard (s == s') >> return s')

parseCmdLine :: Parsec [String] x (Action, [Query])
parseCmdLine = do
  q <- parseQueries <|> return [InSeq "cur"]
  a <- parseAction
  eof
  return (a, q)

parseAction = choice $ map try
  [ do expect "show"; return Show
  , do expect "set"; s <- get; return $ SetSeq s
  , do expect "add"; s <- get; return $ AddToSeq s
  , do expect "del"; s <- get; return $ RemoveFromSeq s
  , return List
  ]

parseQuery = choice $ map try
  [ do expect "in"; s <- get; return $ InSeq s
  , do expect "from"; s <- get; return $ WithHeader "from" s
  , do expect "to"; s <- get; return $ WithHeader "to" s
  , do expect "cc"; s <- get; return $ WithHeader "cc" s
  , do expect "subject"; s <- get; return $ WithHeader "subject" s
  , do expect "subj"; s <- get; return $ WithHeader "subject" s
  , do expect "list"; s <- get; return $ WithHeader "list-id" s
  , do expect "header"; h <- get; v <- get; return $ WithHeader h v
  , do expect "personal"; return $ Not $ WithHeader "list-id" ""
  , do expect "new"; return $ Not $ InSeq "seen"
  , do expect "old"; return $ InSeq "seen"
  , do expect "refine"; return $ InSeq "cur"
  , do expect "("; qs <- parseQueries; expect ")"; return $ And qs
  , do expect "not"; q <- parseQuery; return $ Not q
  , do expect "any"; qs <- parseQueries; return $ Or qs
  ]

parseQueries = many1 parseQuery <|> (do try $ expect "all"; return [])
