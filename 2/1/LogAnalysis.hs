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
