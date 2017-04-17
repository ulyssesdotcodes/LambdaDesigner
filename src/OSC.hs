{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module OSC where

import Control.Monad.Trans.State.Lazy
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD as T

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

import Op


type Messages = Trie [Message]

data MessageType = Create
                 | Connect
                 | Parameter

instance Datem MessageType where
  d_put Create = ASCII_String "create"
  d_put Connect = ASCII_String "connect"
  d_put Parameter = ASCII_String "parameter"
  d_get _ = Nothing -- Not used

parseTree :: (Monad m) => Tree a -> StateT Messages m (String, [Message])
parseTree (MovieFileInTree top) = op1Messages top
parseTree (DisplaceTree top op1 op2) = op2Messages top op1 op2
parseTree (NoiseCHOPTree chop) = op1Messages chop
parseTree (CHOPToTree top) = op1Messages top

parseParam :: (Monad m) => Param a -> StateT Messages m (BS.ByteString, [Message])
parseParam (File val) = do return (val, [])

parseParam (CHOPPar chop) = do (paddr, pmsgs) <- parseTree chop
                               return (BS.pack $ "op(\"" ++ tail paddr ++ "\")", pmsgs)

parseParam (CHOPOpPar choppar) = parseParam choppar
parseParam (ShowF f) = parseParam f
parseParam (F f) = pure (BS.pack $ show f, [])
parseParam Seconds = pure ("absTime.seconds", [])
parseParam (Mult a b) = do (abs, amsgs) <- parseParam a
                           (bbs, bmsgs) <- parseParam b
                           return (BS.concat [abs, " * ", bbs], amsgs ++ bmsgs)



op1Messages :: (Monad m, Op a) => a -> StateT Messages m (String, [Message])
op1Messages a = do messages <- get
                   let ty = opType a
                   let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                   let addr = "/" ++ (BS.unpack . BS.append ty $ findEmpty nodesOfType)
                   let createMessage = Message addr [d_put Create, d_put ty]
                   modify $ insert (BS.pack addr) [createMessage]
                   parsMessages <- sequence $ M.foldrWithKey (\k p parstates -> (do (val, msgs) <- parseParam p
                                                                                    let msg = Message addr [d_put Parameter, ASCII_String k, ASCII_String val]
                                                                                    modify $ adjust ((:) msg) (BS.pack addr)
                                                                                    return (msg:msgs)):parstates) [] (opPars a)
                   return (addr, createMessage:(concat parsMessages))

op2Messages :: (Monad m, Op a) => a -> Tree a -> Tree a -> StateT Messages m (String, [Message])
op2Messages a op1 op2 = do (addr1, ms1) <- parseTree op1
                           (addr2, ms2) <- parseTree op2
                           (addr, ms) <- op1Messages a
                           let connect1 = Message addr [d_put Connect, int32 0, string addr1]
                           let connect2 = Message addr [d_put Connect, int32 1, string addr2]
                           let connects = [connect1, connect2]
                           modify $ insert (BS.pack addr) connects
                           return $ (addr, ms ++ ms1 ++ ms2 ++ connects)

findEmpty :: Trie [Message] -> BS.ByteString
findEmpty = BS.pack . show . length . keys

sendMessages :: UDP -> [Message] -> IO ()
sendMessages conn ms = T.sendOSC conn $ Bundle 0 $ ms
