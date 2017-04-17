{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module OSC where

import Control.Monad.Trans.State.Lazy
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M

import Op

data Messagable = Create BS.ByteString
                | Connect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString deriving Eq

instance Ord Message where
  (<=) (Message _ ((ASCII_String "create"):_)) _ = True
  (<=) _ _ = False

type Messages = Trie [Messagable]

-- data MessageType = Create
--                  | Connect
--                  | Parameter

-- instance Datem MessageType where
--   d_put Create = ASCII_String "create"
--   d_put Connect = ASCII_String "connect"
--   d_put Parameter = ASCII_String "parameter"
--   d_get _ = Nothing -- Not used

parseTree :: (Monad m) => Tree a -> StateT Messages m String
parseTree (MovieFileInTree top) = op0Messages top
parseTree (DisplaceTree top op1 op2) = op2Messages top op1 op2
parseTree (NoiseCHOPTree chop) = op0Messages chop
parseTree (CHOPToTree top) = op0Messages top
parseTree (OutTOPTree top op1) = op1Messages top op1
parseTree (OutSOPTree sop op1) = op1Messages sop op1
parseTree (SphereTree s) = op0Messages s

parseParam :: (Monad m) => Param a -> StateT Messages m BS.ByteString
parseParam (File val) = do return val

parseParam (CHOPPar chop) = do paddr <- parseTree chop
                               return $ BS.pack $ "op(\"" ++ tail paddr ++ "\")"

parseParam (CHOPOpPar choppar) = parseParam choppar
parseParam (ShowP f) = parseParam f
parseParam (F f) = pure $ BS.pack $ show f
parseParam (I i) = pure $ BS.pack $ show i
parseParam Seconds = pure $ "absTime.seconds"
parseParam (Mult a b) = do abs <- parseParam a
                           bbs <- parseParam b
                           return $ BS.concat [abs, " * ", bbs]



op0Messages :: (Monad m, Op a) => a -> StateT Messages m String
op0Messages a = do messages <- get
                   let ty = opType a
                   let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                   let addr = "/" ++ (BS.unpack . BS.append ty $ findEmpty nodesOfType)
                   let createMessage = Create ty
                   modify $ insert (BS.pack addr) [createMessage]
                   sequence_ $ M.foldrWithKey (\k p parstates -> (do val <- parseParam p
                                                                     let msg = Parameter k val
                                                                     modify $ adjust ((:) msg) (BS.pack addr)
                                                                     return ()):parstates) [] (opPars a)
                   return addr

op1Messages :: (Monad m, Op a) => a -> Tree a -> StateT Messages m String
op1Messages a op1 = do addr1 <- parseTree op1
                       addr <- op0Messages a
                       let connect = Connect 0 (BS.pack addr1)
                       modify $ adjust ((:) connect) (BS.pack addr)
                       return addr

op2Messages :: (Monad m, Op a) => a -> Tree a -> Tree a -> StateT Messages m String
op2Messages a op1 op2 = do addr <- op1Messages a op1
                           addr2 <- parseTree op2
                           let connect2 = Connect 1 (BS.pack addr2)
                           modify $ adjust ((:) connect2) (BS.pack addr)
                           return addr

findEmpty :: Trie [Messagable] -> BS.ByteString
findEmpty = BS.pack . show . length . keys

makeMessages :: Trie [Messagable] -> [Message]
makeMessages = L.sort . allMsgs . toList
               where
                 allMsgs ((addr, msgs):ms) = (addrMsgs addr msgs) ++ allMsgs ms
                 allMsgs [] = []
                 addrMsgs addr ((Create ty):ms) = (Message (BS.unpack addr) [string "create", ASCII_String ty]):(addrMsgs addr ms)
                 addrMsgs addr ((Connect i caddr):ms) = (Message (BS.unpack addr) [string "connect", int32 i, ASCII_String caddr]):(addrMsgs addr ms)
                 addrMsgs addr ((Parameter k v):ms) = (Message (BS.unpack addr) [string "parameter", ASCII_String k, ASCII_String v]):(addrMsgs addr ms)
                 addrMsgs addr [] = []


sendMessages :: UDP -> [Message] -> IO ()
sendMessages conn ms = T.sendOSC conn $ Bundle 0 $ ms
