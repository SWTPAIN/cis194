{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Maybe
import Log

parseMessage :: String -> LogMessage
parseMessage message =
  let wordList = words message
  in
  case wordList of
    ("I":ts:msg)    -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg)    -> LogMessage Warning (read ts) (unwords msg)
    ("E":lv:ts:msg) -> LogMessage (Error (read lv)) (read ts) (unwords msg)
    _               -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg@LogMessage{} Leaf = Node Leaf logMsg Leaf
insert logMsg1@(LogMessage _ ts1 _) (Node left logMsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left logMsg2 (insert logMsg1 right)
  | otherwise = Node (insert logMsg1 left) logMsg2 right
insert _ msgTree = msgTree

build :: [LogMessage] -> MessageTree
build []                = Leaf
build (logMsg:[])       = (Node Leaf logMsg Leaf)
build (logMsg:logMsgs)  = (insert logMsg (build logMsgs))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMsg rightTree) = (inOrder leftTree) ++ [logMsg] ++ (inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = catMaybes . map extractMessage . inOrder . build . filter (severe 50)
  where
    severe :: Int -> LogMessage -> Bool
    severe minLv (LogMessage (Error lv) _ _)
      | lv > minLv = True
      | otherwise = False
    severe _ _ = False

extractMessage :: LogMessage -> Maybe String
extractMessage (LogMessage _ _ msg) = Just msg
extractMessage _                    = Nothing
