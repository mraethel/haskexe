{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String  -> LogMessage
parseMessage l = case l of
               ('I':' ':s) -> LogMessage Info ((read . head . words) s) ((unwords . tail . words) s)
               ('W':' ':s) -> LogMessage Warning ((read . head . words) s) ((unwords . tail . words) s)
               ('E':' ':s) -> LogMessage (Error ((read . head . words) s)) ((read . head . (drop 1) . words) s) ((unwords . tail . (drop 1)  . words) s)
               s -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt (LogMessage _ cts _) rmt)
  | ts < cts = Node (insert lm lmt) nlm rmt
  | otherwise = Node lmt nlm (insert lm rmt)
insert (Unknown _) mt = mt
insert _ mt = mt
