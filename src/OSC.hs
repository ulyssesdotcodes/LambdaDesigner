{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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

data CommandType = Pulse BS.ByteString deriving Eq

data Messagable = Create BS.ByteString
                | Connect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString
                | TextContent BS.ByteString
                | Command CommandType
                deriving Eq


instance Ord Message where
  compare (Message (length . filter (=='/') -> counta) ((ASCII_String "create"):_)) (Message (length . filter (=='/') -> countb) ((ASCII_String "create"):_)) = compare counta countb
  compare (Message _ ((ASCII_String "create"):_)) _ = LT
  compare _ (Message _ ((ASCII_String "create"):_)) = GT
  compare _ _ = EQ

type Messages = Trie [Messagable]

parseTree :: (Monad m, Op a) => Tree a -> StateT Messages m String
parseTree (GeneratorTree a) = op0Messages a
parseTree (EffectTree a aop) = op1Messages a aop
parseTree (CompositeTree top op1 op2) = op2Messages top op1 op2
parseTree (ComponentTree c bop) = do addr <- op0Messages c
                                     tr <- execStateT (parseTree bop) empty
                                     let modMsg ((Connect i a):ms) = (Connect i (BS.concat [BS.pack addr, a])):(modMsg ms)
                                         modMsg (m:ms) = m:(modMsg ms)
                                         modMsg [] = []
                                     modify $ unionR . fromList . fmap (\(a, ms) -> (BS.concat [BS.pack addr, a], modMsg ms)) . toList $ tr
                                     return addr

parseTree (FeedbackTree top input transform composite) = do messages <- get
                                                            fbaddr <- evalStateT (op0Messages top) messages
                                                            inaddr <- evalStateT (parseTree input) messages
                                                            let transformTree = transform $ GeneratorTree top
                                                                compTree = composite transformTree input
                                                            compAddr <- parseTree compTree
                                                            let connect = Connect 0 (BS.pack inaddr)
                                                                par = Parameter "top" (BS.pack $ "op(\"" ++ tail compAddr ++ "\")")
                                                            let resetMsg = Command $ Pulse "reset"
                                                            modify $ adjust ((++) [resetMsg, connect, par]) (BS.pack fbaddr) -- Hacky but works because evalStateT is pure
                                                            modify $ adjust ((:) connect) (BS.pack compAddr)
                                                            return compAddr

parseParam :: (Monad m) => Param a -> StateT Messages m BS.ByteString
parseParam (File val) = do return val
parseParam (TreeFloat mod a) = do opstring <- parseParam a
                                  return $ mod opstring
parseParam (ShowP f) = parseParam f
parseParam (F f) = pure $ BS.pack $ show f
parseParam (I i) = pure $ BS.pack $ show i
parseParam (S s) = pure $ BS.pack $ "\"" ++ s ++ "\""
parseParam (B b) = pure $ BS.pack $ if b then show 1 else show 0
parseParam Seconds = pure $ "absTime.seconds"
parseParam (Sin a) = parseParam a >>= \s -> return $ BS.concat ["math.sin(", s, ")"]
-- parseParam (Sin' a) = parseParam (Add (F 0.5) $ Mult (F 0.5) (Sin a))
parseParam (Mult a b) = makeExpr a b "*"
parseParam (Add a b) = makeExpr a b "+"

parseParam (TreePar tree) = do paddr <- parseTree tree
                               return $ BS.pack $ "op(\"" ++ tail paddr ++ "\")"

makeExpr :: (Monad m, Floating f) => Param f -> Param f -> BS.ByteString -> StateT Messages m BS.ByteString
makeExpr a b op = do abs <- parseParam a
                     bbs <- parseParam b
                     return $ BS.concat ["(", abs," " ,op ," " , bbs, ")"]

op0Messages :: (Monad m, Op a) => a -> StateT Messages m String
op0Messages a = do messages <- get
                   let ty = opType a
                   let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                   let addr = "/" ++ (BS.unpack . BS.append ty $ findEmpty nodesOfType)
                   let createMessage = Create ty
                   let textMessage =
                         case opText a of
                           Just content -> [TextContent content]
                           Nothing -> []
                   modify $ insert (BS.pack addr) (createMessage:textMessage)
                   sequence_ $ M.foldrWithKey (\k p parstates -> (do val <- parseParam p
                                                                     let msg = Parameter k val
                                                                     modify $ adjust ((:) msg) (BS.pack addr)
                                                                     return ()):parstates) [] (opPars a)
                   removeDuplicates addr ty

op1Messages :: (Monad m, Op a) => a -> Tree a -> StateT Messages m String
op1Messages a op1 = do addr1 <- parseTree op1
                       addr <- op0Messages a
                       let connect = Connect 0 (BS.pack addr1)
                       modify $ adjust ((:) connect) (BS.pack addr)
                       removeDuplicates addr (opType a)

op2Messages :: (Monad m, Op a) => a -> Tree a -> Tree a -> StateT Messages m String
op2Messages a op1 op2 = do addr <- op1Messages a op1
                           addr2 <- parseTree op2
                           let connect2 = Connect 1 (BS.pack addr2)
                           modify $ adjust ((:) connect2) (BS.pack addr)
                           return addr

removeDuplicates :: (Monad m) => String -> BS.ByteString -> StateT Messages m String
removeDuplicates addr ty = do messages <- get
                              let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                              let addrMsgs = lookup (BS.pack addr) messages
                              -- If messages are all the same then they're equivalent so we can combine the nodes
                              case filter (\(a, ms) -> a /= (BS.pack addr) && addrMsgs == Just ms) (toList nodesOfType) of
                                ((maddr, _):_) -> do modify . delete . BS.pack $ addr
                                                     return (BS.unpack maddr)
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
                 addrMsgs _ [] = []


sendMessages :: UDP -> [Message] -> IO ()
sendMessages conn ms = T.sendOSC conn $ Bundle 0 $ ms
