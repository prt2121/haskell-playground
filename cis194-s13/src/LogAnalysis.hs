{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
parseMessage :: String -> LogMessage
parseMessage = p . words
               where
                p ("I" : t : str)       = LogMessage Info (read t) $ unwords str
                p ("W" : t : str)       = LogMessage Warning (read t) $ unwords str
                p ("E" : n : t : str)   = LogMessage (Error (read n)) (read t) $ unwords str
                p str                   = Unknown $ unwords str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t                                                        = t
insert m@(LogMessage _ _ _) Leaf                                            = Node Leaf m Leaf
insert m@(LogMessage _ time _) (Node left m2@(LogMessage _ time2 _) right)  = if time >= time2 then Node left m2 (insert m right) else Node (insert m left) m2 right
insert _ t                                                                  = t

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node left m right) = inOrder left ++ m : (inOrder right)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter isRelevant . inOrder . build
  where
        message (LogMessage _ _ m)                      = m
        message (Unknown m)                             = m
        isRelevant (LogMessage (Error severity) _ _)    = severity >= 50
        isRelevant _                                    = False