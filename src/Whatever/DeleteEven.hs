module Whatever.DeleteEven where

import System.Environment
import System.IO

data LinkedListNode a = Nil | LinkedListNode {next :: LinkedListNode a, value :: a}

instance Show (LinkedListNode String) where
    show Nil = ""
    show (LinkedListNode Nil v) = v
    show (LinkedListNode ptr v) = v ++ "\n" ++ show ptr

getLinkedList :: [a] -> LinkedListNode a
getLinkedList [] = Nil
getLinkedList (x:xs) = LinkedListNode {next = getLinkedList xs, value = x}

deleteEven :: LinkedListNode Int -> LinkedListNode Int
deleteEven Nil = Nil
deleteEven (LinkedListNode {next=next, value=value})
  | even value = deleteEven next
  | otherwise = LinkedListNode { next=(deleteEven next), value=value}
