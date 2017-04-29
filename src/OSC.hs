{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module OSC where

import Prelude hiding (lookup)

import Op

import Control.Monad.Trans.State.Lazy
import Control.Lens
import Data.Maybe
import Sound.OSC
import Sound.OSC.Transport.FD as OT

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.Map.Strict as M
import Data.Trie as T

data Messagable = Create BS.ByteString
                | Connect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString
                | TextContent BS.ByteString
                | Command CommandType
                | Fixed BS.ByteString
                deriving Eq

type Messages = Trie [Messagable]


instance Ord Message where
  compare (Message (L.length . L.filter (=='/') -> counta) ((ASCII_String "create"):_)) (Message (L.length . L.filter (=='/') -> countb) ((ASCII_String "create"):_)) = compare counta countb
  compare (Message _ ((ASCII_String "create"):_)) _ = LT
  compare _ (Message _ ((ASCII_String "create"):_)) = GT
  compare (Message _ ((ASCII_String "connect"):(Int32 i):_)) (Message _ ((ASCII_String "connect"):(Int32 i2):_)) = compare i i2
  compare (Message _ ((ASCII_String "command"):_)) _ = GT
  compare _ _ = EQ

parseTree :: (Monad m) => Tree a -> StateT Messages m ByteString
parseTree (C p) = opsMessages p
parseTree (PyExprF c) = pure c
parseTree (PyExprI c) = pure c
parseTree (Mod f ta) = do aaddr <- parseTree ta
                          return $ f aaddr
parseTree (Mod2 f ta tb) = do aaddr <- parseTree ta
                              baddr <- parseTree tb
                              return $ f aaddr baddr


opsMessages :: (Monad m, Op a) => a -> StateT Messages m BS.ByteString
opsMessages a = do let ty = opType a
                   messages <- get
                   let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                   let addr = BS.concat ["/", ty, findEmpty nodesOfType]
                   let createMessage = Create ty
                   let textMessage =
                         case text a of
                           Just content -> [TextContent content]
                           Nothing -> []
                   let commandMessages = Prelude.map Command $ commands a
                   modify $ T.insert addr (createMessage:textMessage ++ commandMessages)
                   mapM_ (\(k, p) -> do val <- parseTree p
                                        let msg = Parameter k val
                                        modify $ T.adjust ((:) msg) addr
                                        return ()) (pars a)
                   mapM_ (\(i, op) -> do a <- parseTree op
                                         let connect = Connect i a
                                         modify $ T.adjust ((:) connect) addr
                                         return a) . Prelude.zip [0..] $ connections a
                   removeDuplicates addr (opType a)

removeDuplicates :: (Monad m) => BS.ByteString -> BS.ByteString -> StateT Messages m BS.ByteString
removeDuplicates addr ty = do messages <- get
                              let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                              let addrMsgs = T.lookup addr messages
                              -- If messages are all the same then they're equivalent so we can combine the nodes
                              case L.filter (\(a, ms) -> a /= addr && addrMsgs == Just ms) (T.toList nodesOfType) of
                                ((maddr, _):_) -> do modify . T.delete $ addr
                                                     return maddr
                                _ -> return addr

findEmpty :: Messages -> BS.ByteString
findEmpty = pack . show . L.length . T.keys

makeMessages :: Messages -> [Message]
makeMessages = L.sort . allMsgs . T.toList
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
sendMessages conn ms = OT.sendOSC conn $ Bundle 0 $ ms
