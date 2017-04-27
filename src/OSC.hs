{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module OSC where

import Prelude hiding (lookup)

import Op
import Tree

import Control.Monad.Trans.State.Lazy
import Control.Lens
import Data.Maybe
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M


instance Ord Message where
  compare (Message (length . filter (=='/') -> counta) ((ASCII_String "create"):_)) (Message (length . filter (=='/') -> countb) ((ASCII_String "create"):_)) = compare counta countb
  compare (Message _ ((ASCII_String "create"):_)) _ = LT
  compare _ (Message _ ((ASCII_String "create"):_)) = GT
  compare (Message _ ((ASCII_String "connect"):(Int32 i):_)) (Message _ ((ASCII_String "connect"):(Int32 i2):_)) = compare i i2
  compare (Message _ ((ASCII_String "command"):_)) _ = GT
  compare _ _ = EQ


makeExpr :: (Monad m, Num f) => Param f -> Param f -> BS.ByteString -> StateT Messages m BS.ByteString
makeExpr a b op = do abs <- parseParam a
                     bbs <- parseParam b
                     return $ BS.concat ["(", abs," " ,op ," " , bbs, ")"]

op0Messages :: (Monad m, Op a) => a -> StateT Messages m BS.ByteString
op0Messages a = opsMessages a []

op1Messages :: (Monad m, Op a) => a -> Tree a -> StateT Messages m BS.ByteString
op1Messages a op1 = opsMessages a [op1]

op2Messages :: (Monad m, Op a) => a -> Tree a -> Tree a -> StateT Messages m BS.ByteString
op2Messages a op1 op2 = opsMessages a [op1, op2]

opsMessages :: (Monad m, Op a) => a -> [Tree a] -> StateT Messages m BS.ByteString
opsMessages a ops = do let ty = opType a
                       messages <- get
                       let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                       let addr = BS.concat ["/", ty, findEmpty nodesOfType]
                       let createMessage = Create ty
                       let textMessage =
                             case opText a of
                               Just content -> [TextContent content]
                               Nothing -> []
                       let commandMessages = map Command $ opCommands a
                       modify $ insert addr (createMessage:textMessage ++ commandMessages)
                       sequence_ $ M.foldrWithKey (\k p parstates -> (do val <- parseParam p
                                                                         let msg = Parameter k val
                                                                         modify $ adjust ((:) msg) addr
                                                                         return ()):parstates) [] (opPars a)
                       sequence_ . map (\(i, op) -> do a <- parseTree op
                                                       let connect = Connect i a
                                                       modify $ adjust ((:) connect) addr
                                                       return a) . zip [0..] $ ops
                       removeDuplicates addr (opType a)

removeDuplicates :: (Monad m) => BS.ByteString -> BS.ByteString -> StateT Messages m BS.ByteString
removeDuplicates addr ty = do messages <- get
                              let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                              let addrMsgs = lookup addr messages
                              -- If messages are all the same then they're equivalent so we can combine the nodes
                              case filter (\(a, ms) -> a /= addr && addrMsgs == Just ms) (toList nodesOfType) of
                                ((maddr, _):_) -> do modify . delete $ addr
                                                     return maddr
                                _ -> return addr

findEmpty :: Messages -> BS.ByteString
findEmpty = BS.pack . show . length . keys

makeMessages :: Messages -> [Message]
makeMessages = L.sort . allMsgs . toList
               where
                 allMsgs ((addr, msgs):ms) = (addrMsgs addr msgs) ++ allMsgs ms
                 allMsgs [] = []
                 addrMsgs addr ((Create ty):ms) = (Message (BS.unpack addr) [string "create", ASCII_String ty]):(addrMsgs addr ms)
                 addrMsgs addr ((Connect i caddr):ms) = (Message (BS.unpack addr) [string "connect", int32 i, ASCII_String caddr]):(addrMsgs addr ms)
                 addrMsgs addr ((Parameter k v):ms) = (Message (BS.unpack addr) [string "parameter", ASCII_String k, ASCII_String v]):(addrMsgs addr ms)
                 addrMsgs addr ((Command (Pulse k)):ms) = (Message (BS.unpack addr) [string "command", string "pulse", ASCII_String k]):(addrMsgs addr ms)
                 addrMsgs addr ((TextContent content):ms) = (Message (BS.unpack addr) [string "text", ASCII_String content]):(addrMsgs addr ms)
                 addrMsgs addr ((Fixed _):ms) = addrMsgs addr ms
                 addrMsgs _ [] = []


sendMessages :: UDP -> [Message] -> IO ()
sendMessages conn ms = T.sendOSC conn $ Bundle 0 $ ms
