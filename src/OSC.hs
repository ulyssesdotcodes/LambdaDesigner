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
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Messagable = Create BS.ByteString
                | Connect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString
                | TextContent BS.ByteString
                | Command CommandType
                | Fixed BS.ByteString
                deriving Eq


instance Ord Message where
  compare (Message (length . filter (=='/') -> counta) ((ASCII_String "create"):_)) (Message (length . filter (=='/') -> countb) ((ASCII_String "create"):_)) = compare counta countb
  compare (Message _ ((ASCII_String "create"):_)) _ = LT
  compare _ (Message _ ((ASCII_String "create"):_)) = GT
  compare (Message _ ((ASCII_String "connect"):(Int32 i):_)) (Message _ ((ASCII_String "connect"):(Int32 i2):_)) = compare i i2
  compare (Message _ ((ASCII_String "command"):_)) _ = GT
  compare _ _ = EQ

type Messages = Trie [Messagable]

parseTree :: (Monad m, Op a) => Tree a -> StateT Messages m BS.ByteString
parseTree (GeneratorTree a) = op0Messages a
parseTree (EffectTree a aop) = op1Messages a aop
parseTree (CombineTree top op1 op2) = op2Messages top op1 op2
parseTree (CompositeTree c ops) = opsMessages c ops
parseTree (ComponentTree c bop) = do addr <- op0Messages c
                                     tr <- execStateT (parseTree bop) empty
                                     let modMsg ((Connect i a):ms) = (Connect i (BS.concat [addr, a])):(modMsg ms)
                                         modMsg (m:ms) = m:(modMsg ms)
                                         modMsg [] = []
                                     modify $ unionR . fromList . fmap (\(a, ms) -> (BS.concat [addr, a], modMsg ms)) . toList $ tr
                                     return addr
parseTree (FixedTree name op) = do messages <- get
                                   case lookup name messages of
                                     Just [Fixed addr] -> return addr

                                     Nothing -> do addr <- parseTree op
                                                   modify $ insert name [Fixed addr]
                                                   return addr

parseTree (FeedbackTree top reset transform sel) = do messages <- get
                                                      fbaddr <- evalStateT (op0Messages top) messages
                                                      resetaddr <- parseTree reset
                                                      let transformTree = transform $ GeneratorTree top
                                                      transformAddr <- evalStateT (parseTree transformTree) messages
                                                      seladdr <- parseTree $ sel transformTree
                                                      if | opType top == opType FeedbackTOP -> do let par = Parameter "top" (BS.concat ["op(\"", BS.tail seladdr, "\")"])
                                                                                                      connect = Connect 0 (resetaddr)
                                                                                                  modify $ adjust ((++) [par, connect]) (fbaddr)
                                                         | opType top == opType FeedbackCHOP -> do let connectsel = Connect 0 (seladdr)
                                                                                                       connectreset = Connect 1 (resetaddr)
                                                                                                   modify $ adjust ((++) [connectsel, connectreset]) (fbaddr)
                                                      let resetMsg = Command $ Pulse "reset"
                                                      modify $ adjust ((:) resetMsg) (fbaddr) -- Hacky but works because evalStateT is pure
                                                      return transformAddr

parseParam :: (Monad m) => Param a -> StateT Messages m BS.ByteString
parseParam (File val) = do return val
parseParam (TreeFloat mod a) = do opstring <- parseParam a
                                  return $ mod opstring
parseParam (TreeString mod a) = do opstring <- parseParam a
                                   return $ mod opstring
parseParam (ShowP f) = parseParam f
parseParam (F f) = pure $ BS.pack $ show f
parseParam (I i) = pure $ BS.pack $ show i
parseParam (S s) = pure $ BS.pack $ "\"" ++ s ++ "\""
parseParam (B b) = pure $ BS.pack $ if b then show 1 else show 0
parseParam (MakeFloat i) = parseParam i
parseParam (PyExpr s) = pure s
parseParam (Mult a b) = makeExpr a b "*"
parseParam (Add a b) = makeExpr a b "+"
parseParam (Mod f a) = parseParam a >>= \s -> return $ f s
parseParam (Mod2 f a b) = parseParam a >>= \s -> parseParam b >>= \t -> return $ f s t
parseParam (Cell a b t) = do ta <- parseParam t
                             aa <- parseParam a
                             ab <- parseParam b
                             return $ BS.concat [ta, "[", aa, ",", ab, "]"]
parseParam (TreePar tree) = do paddr <- parseTree tree
                               return $ BS.concat ["op(\"", BS.tail paddr, "\")"]

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
