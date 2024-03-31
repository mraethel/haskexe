{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String  -> LogMessage
parseMessage l = case l of
               ('I':' ':s) -> LogMessage Info ((read . head . words) s) ((unwords . tail . words) s)
               ('W':' ':s) -> LogMessage Warning ((read . head . words) s) ((unwords . tail . words) s)
               ('E':' ':s) -> LogMessage (Error ((read . head . words) s)) ((read . head . (drop 1) . words) s) ((unwords . tail . (drop 1) . words) s)
               s -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node lmt nlm@(LogMessage _ cts _) rmt)
  | ts < cts = Node (insert lm lmt) nlm rmt
  | otherwise = Node lmt nlm (insert lm rmt)
insert _ mt = mt

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm (build lms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lmt lm rmt) = (inOrder lmt) ++ [lm] ++ (inOrder rmt)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lms = map toString ((inOrder . build . (filter sev50)) lms)

sev50 :: LogMessage -> Bool
sev50 (LogMessage (Error s) _ _)
  | s > 50 = True
  | otherwise = False
sev50 _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ s) = s
toString (Unknown s) = s
