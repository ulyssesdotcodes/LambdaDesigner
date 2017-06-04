{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OSC where

import Prelude hiding (lookup)

import Op

import Control.Lens
import Control.Lens.Cons
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Data.Maybe
import Data.Text.Encoding
import GHC.Generics
import Sound.OSC
import Sound.OSC.Transport.FD as OT

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BSL
import Data.List as L
import Data.List.Lens
import Data.Map.Strict as M
import Data.Trie as T

import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Text as Tx

data Messagable = Create BS.ByteString
                | Connect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString
                | CustomPar BS.ByteString BS.ByteString
                | TextContent BS.ByteString
                | Command BS.ByteString [BS.ByteString]
                | Fixed BS.ByteString
                deriving (Eq, Show)

data JSONNode = JSONNode { _nodeType :: Tx.Text
                         , _nodeConnections :: [(Int, Tx.Text)]
                         , _nodeParameters :: Map Tx.Text Tx.Text
                         , _nodeCommands :: [(Tx.Text, [Tx.Text])]
                         , _nodeText :: Maybe Tx.Text
                         }
  deriving Generic

makeLenses ''JSONNode
emptyJsonNode = JSONNode "" [] mempty [] Nothing

instance A.ToJSON JSONNode where
  toJSON (JSONNode {..}) = A.object [ "ty" A..= _nodeType
                                    , "connections" A..= (connsvalue _nodeConnections)
                                    , "parameters" A..= _nodeParameters
                                    , "commands" A..= comsvalue _nodeCommands
                                    , "text" A..= _nodeText
                                    ]
    where
      connsvalue cs = V.replicate (L.length cs) "" V.// cs
      comsvalue = V.fromList . L.map (\a -> A.object ["command" A..= fst a, "args" A..= V.fromList (snd a)])
  toEncoding = A.genericToEncoding A.defaultOptions


type Messages = Trie [Messagable]


instance Ord Message where
  compare (Message (L.length . L.filter (=='/') -> counta) ((ASCII_String "create"):_)) (Message (L.length . L.filter (=='/') -> countb) ((ASCII_String "create"):_)) = compare counta countb
  compare (Message _ ((ASCII_String "create"):_)) _ = LT
  compare _ (Message _ ((ASCII_String "create"):_)) = GT
  compare (Message _ ((ASCII_String "custompar"):_)) _ = GT
  compare _ (Message _ ((ASCII_String "custompar"):_)) = LT
  compare (Message _ ((ASCII_String "command"):_)) _ = GT
  compare _ (Message _ ((ASCII_String "command"):_)) = LT
  compare (Message _ ((ASCII_String "connect"):(Int32 i):_)) (Message _ ((ASCII_String "connect"):(Int32 i2):_)) = compare i i2
  compare _ _ = EQ

parseParam :: (Monad m) => Tree a -> StateT Messages m BS.ByteString
parseParam t@(N p) = parseTree t >>= return . wrapOp
parseParam t@(Comp {}) = parseTree t >>= return . wrapOp
parseParam t@(FC {}) = parseTree t >>= return . wrapOp
parseParam t@(FT {}) = parseTree t >>= return . wrapOp
parseParam t@(Fix {}) = parseTree t >>= return . wrapOp
parseParam t = parseTree t

wrapOp :: BS.ByteString -> BS.ByteString
wrapOp op = BS.concat ["op(\"", BS.tail op, "\")"]

parseTree :: (Monad m) => Tree a -> StateT Messages m BS.ByteString
parseTree (N p) = opsMessages p
parseTree (Comp p child) = do addr <- opsMessages p
                              tr <- execStateT (parseTree child) T.empty
                              let modMsg ((Connect i a):ms) = (Connect i (BS.concat [addr, a])):(modMsg ms)
                                  modMsg (m:ms) = m:(modMsg ms)
                                  modMsg [] = []
                              modify $ unionR . T.fromList . fmap (\(a, ms) -> (BS.concat [addr, a], modMsg ms)) . T.toList $ tr
                              return addr
parseTree (BComp p f a) = do addr <- opsMessages p
                             aaddr <- parseTree a
                             let inNode = inOp
                                 outNode = outOp
                             tr <- execStateT (parseTree $ outNode $ f inNode) T.empty
                             let modMsg ((Connect i a):ms) = (Connect i (BS.concat [addr, a])):(modMsg ms)
                                 modMsg (m:ms) = m:(modMsg ms)
                                 modMsg [] = []
                             modify $ unionR . T.fromList . fmap (\(a, ms) -> (BS.concat [addr, a], modMsg ms)) . T.toList $ tr
                             modify $ T.adjust ((:) (Connect 0 aaddr)) addr
                             return addr
parseTree (Tox p mf) = do addr <- opsMessages p
                          case parseTree <$> mf of
                            Just c -> do caddr <- c
                                         modify $ T.adjust ((:) (Connect 0 caddr)) addr
                                         return addr
                            Nothing -> return addr
parseTree (FC fpars reset loop sel) = do messages <- get
                                         saddr <- evalStateT (parseTree $ N (SelectCHOP Nothing)) messages
                                         let sname = BS.append "fb_" $ BS.tail saddr
                                         laddr <- parseTree (loop $ N $ fpars & chopIns .~ [sel $ fix sname $ N (SelectCHOP Nothing), reset])
                                         modify $ T.adjust ((:) (Parameter "chop" $ wrapOp laddr)) $ BS.append "/" sname
                                         return laddr
parseTree (FT fpars reset loop sel) = do messages <- get
                                         saddr <- evalStateT (parseTree $ N (SelectTOP Nothing)) messages
                                         let sname = BS.append "fb_" $ BS.tail saddr
                                         laddr <- parseTree (loop $ N $ fpars & topIns .~ [reset] & fbTop ?~ sel (fix sname (N $ SelectTOP Nothing)))
                                         modify $ T.adjust ((:) (Parameter "top" $ wrapOp laddr)) $ BS.append "/" sname
                                         return laddr
parseTree (Fix name op) = do messages <- get
                             let name' = BS.append "/" name
                             case T.member name' messages of
                               True -> return name'
                               False -> do addr <- parseTree op
                                           messages' <- get
                                           modify $ T.insert name' . ((:) (Fixed name)) . fromJust $ T.lookup addr messages'
                                           modify $ T.delete addr
                                           return name'

parseTree (PyExpr s) = pure s
parseTree (ChopChan n c) = do addr <- parseParam c
                              return $ BS.concat [addr, "[", n, "]"]
parseTree (Cell (r, c) t) = do addr <- parseParam t
                               r' <- parseParam r
                               c' <- parseParam c
                               return $ BS.concat [addr, "[", r', ",", c', "]"]
parseTree (NumRows t) = do addr <- parseParam t
                           return $ BS.concat [addr, ".numRows"]
parseTree (Mod f ta) = do aaddr <- parseParam ta
                          return . f $ aaddr
parseTree (Mod2 f ta tb) = do aaddr <- parseParam ta
                              baddr <- parseParam tb
                              return $ f aaddr baddr
parseTree (Cast f a) = do aaddr <- parseParam a
                          return $ f aaddr
parseTree (Resolve r) = parseTree r
parseTree (ResolveP r) = parseParam r

parseCommand :: (Monad m) => CommandType -> StateT Messages m Messagable
parseCommand (Pulse bs v f) = pure $ Command "pulse" [bs, v, BS.pack $ show f]
parseCommand (Store bs t) = do ttype <- parseParam t
                               return $ Command "store" [bs, ttype]


opsMessages :: (Monad m, Op a) => a -> StateT Messages m BS.ByteString
opsMessages a = do let ty = opType a
                   messages <- get
                   let addr = findEmpty ty messages
                   let createMessage = Create ty
                   let textMessage =
                         case text a of
                           Just content -> [TextContent content]
                           Nothing -> []
                   modify $ T.insert addr (createMessage:textMessage)
                   mapM_ (\(k, p) -> do val <- parseParam p
                                        let msg = Parameter k val
                                        modify $ T.adjust ((:) msg) addr
                                        return ()) (pars a)
                   mapM_ (\(k, p) -> do val <- parseParam p
                                        let msg = CustomPar k val
                                        modify $ T.adjust ((:) msg) addr
                                        return ()) (customPars a)
                   mapM_ (\(i, op) -> do a <- parseTree op
                                         let connect = Connect i a
                                         modify $ T.adjust ((:) connect) addr
                                         return a) . Prelude.zip [0..] $ connections a
                   mapM_ (\c -> do m <- parseCommand c
                                   modify $ T.adjust ((:) m) addr
                                   return ()) (commands a)
                   addr' <- removeDuplicates addr (opType a)
                   return $ addr'

removeDuplicates :: (Monad m) => BS.ByteString -> BS.ByteString -> StateT Messages m BS.ByteString
removeDuplicates addr ty = do messages <- get
                              let nodesOfType = submap (BS.append (BS.pack "/") ty) messages
                              let addrMsgs = T.lookup addr messages
                              -- If messages are all the same then they're equivalent so we can combine the nodes
                              case L.filter (\(a, ms) -> a /= addr && addrMsgs == Just ms) (T.toList nodesOfType) of
                                ((maddr, _):_) -> do modify . T.delete $ addr
                                                     return maddr
                                _ -> return addr

findEmpty :: BS.ByteString -> Messages -> BS.ByteString
findEmpty ty ms = BS.concat ["/", ty, "_", BS.pack . findKey 0 . L.map BS.unpack . L.sort . T.keys $ submap (BS.append "/" ty) ms]
  where
    findKey n [] = show n
    findKey n (x:xs) = if (L.tail $ L.dropWhile (/= '_') x) == show n then findKey (n + 1) xs else show n

makeMessages :: Messages -> [Message]
-- makeMessages = L.sort . allMsgs . T.toList
--                where
--                  allMsgs ((addr, msgs):ms) = (addrMsgs addr msgs) ++ allMsgs ms
--                  allMsgs [] = []
--                  addrMsgs addr ((Create ty):ms) = (Message (BS.unpack addr) [string "create", ASCII_String ty]):(addrMsgs addr ms)
--                  addrMsgs addr ((Connect i caddr):ms) = (Message (BS.unpack addr) [string "connect", int32 i, ASCII_String caddr]):(addrMsgs addr ms)
--                  addrMsgs addr ((Parameter k v):ms) = (Message (BS.unpack addr) [string "parameter", ASCII_String k, ASCII_String v]):(addrMsgs addr ms)
--                  addrMsgs addr ((CustomPar k v):ms) = (Message (BS.unpack addr) [string "custompar", ASCII_String k, ASCII_String v]):(addrMsgs addr ms)
--                  addrMsgs addr ((Command c bs):ms) = (Message (BS.unpack addr) $ [string "command", ASCII_String c] ++ (ASCII_String <$> bs)):(addrMsgs addr ms)
--                  addrMsgs addr ((TextContent content):ms) = (Message (BS.unpack addr) [string "text", ASCII_String content]):(addrMsgs addr ms)
--                  addrMsgs addr ((Fixed _):ms) = addrMsgs addr ms
--                  addrMsgs _ [] = []

makeMessages msgs = [Message ("/json") [ASCII_String $ BSL.toStrict . A.encode $ A.toJSON . A.object $
                                        L.map (\(k, v) -> decodeUtf8 k A..= jsonNode v) $ T.toList msgs]]

jsonNode :: [Messagable] -> JSONNode
jsonNode = L.foldl modmsg emptyJsonNode
  where
    modmsg jsnode (Create (Tx.pack . BS.unpack -> t)) = jsnode & nodeType .~ t
    modmsg jsnode (Connect i (Tx.pack . BS.unpack -> c)) = jsnode & nodeConnections %~ \cs -> (i, c):cs
    modmsg jsnode (Parameter (Tx.pack . BS.unpack -> k) (Tx.pack . BS.unpack -> v)) =
      jsnode & nodeParameters %~ M.insert k v
    modmsg jsnode (CustomPar (Tx.pack . BS.unpack -> k) (Tx.pack . BS.unpack -> v)) =
      jsnode & nodeParameters %~ M.insert k v
    modmsg jsnode (Command (Tx.pack . BS.unpack -> c) (L.map (Tx.pack . BS.unpack) -> as)) =
      jsnode & nodeCommands %~ \cs -> (c, as):cs
    modmsg jsnode (TextContent (Tx.pack . BS.unpack -> c)) = jsnode & nodeText ?~ c
    modmsg jsnode _ = jsnode

sendMessages :: UDP -> [Message] -> IO ()
sendMessages conn ms = OT.sendOSC conn $ Bundle 0 $ ms
