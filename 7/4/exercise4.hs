{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import JoinList
import Sized
import Editor
import Buffer
import Scrabble

reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id

main = runEditor editor . reify . fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
