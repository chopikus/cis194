{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case (words str) of
                   ("I" : stamp : rest) -> LogMessage Info (read stamp) (unwords rest)
                   ("W" : stamp : rest) -> LogMessage Warning (read stamp) (unwords rest)
                   ("E" : e : stamp : rest) -> LogMessage (Error (read e)) (read stamp) (unwords rest)
                   _ -> Unknown str

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (line : rest) = (parseMessage line) : (parseLines rest)

parse :: String -> [LogMessage]
parse str = parseLines (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ newStamp _) (Node left nodeMsg@(LogMessage _ nodeStamp _) right) = case (newStamp < nodeStamp) of
                                                                                            True -> Node (insert msg left) nodeMsg right
                                                                                            False -> Node left nodeMsg (insert msg right)
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ (msg : (inOrder right))

filter50 :: [LogMessage] -> [LogMessage]
filter50 [] = []
filter50 (x@(LogMessage (Error severity) _ _) : xs) = let filteredXs = filter50 xs in
                                                      case (severity >= 50) of
                                                      True -> x : filteredXs
                                                      False -> filteredXs
filter50 (_ : xs) = filter50 xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = (map show . filter50 . inOrder . build) msgs