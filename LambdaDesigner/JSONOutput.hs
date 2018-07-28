{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaDesigner.JSONOutput where

import Debug.Trace

import LambdaDesigner.Op
import LambdaDesigner.ParsedOps

import Control.Lens
import Control.Lens.Cons
import Control.Monad.Trans.State.Lazy
import Control.Monad
import Data.Maybe
import Data.Text.Encoding
import GHC.Generics

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
                | RevConnect Int BS.ByteString
                | Parameter BS.ByteString BS.ByteString
                | RevParameter BS.ByteString BS.ByteString BS.ByteString
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

nodeCommands :: Lens' JSONNode [(Tx.Text, [Tx.Text])]
nodeCommands = lens _nodeCommands (\a b -> a {_nodeCommands = b})

nodeType :: Lens' JSONNode Tx.Text
nodeType = lens _nodeType (\a b -> a {_nodeType = b})

nodeConnections :: Lens' JSONNode [(Int, Tx.Text)]
nodeConnections = lens _nodeConnections (\a b -> a {_nodeConnections = b})

nodeParameters :: Lens' JSONNode (Map Tx.Text Tx.Text)
nodeParameters = lens _nodeParameters (\a b -> a {_nodeParameters = b})

nodeText :: Lens' JSONNode (Maybe Tx.Text)
nodeText = lens _nodeText (\a b -> a {_nodeText = b})

emptyJsonNode = JSONNode "" [] mempty [] Nothing

selectchoppars = SelectCHOP Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []
selecttoppars = SelectTOP Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

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

parseParam :: (Monad m) => Tree a -> StateT Messages m BS.ByteString
parseParam t@(N p) = parseTree "" t >>= return . wrapOp
parseParam t@(Comp {}) = parseTree "" t >>= return . wrapOp
parseParam t@(FC {}) = parseTree "" t >>= return . wrapOp
parseParam t@(FT {}) = parseTree "" t >>= return . wrapOp
parseParam t@(Fix {}) = parseTree "" t >>= return . wrapOp
parseParam t = parseTree "" t

wrapOp :: BS.ByteString -> BS.ByteString
wrapOp op = BS.concat ["op(\"", BS.tail op, "\")"]

parseTree :: (Monad m) => BS.ByteString -> Tree a -> StateT Messages m BS.ByteString
parseTree pre (N p) = opsMessages pre p
-- parseTree pre (Comp p child) = 
--   do 
--     addr <- opsMessages pre p
--     tr <- execStateT (parseTree pre child) T.empty
--     let modMsg ((Connect i a):ms) = (Connect i (BS.concat [addr, a])):(modMsg ms)
--         modMsg ((RevParameter i a b):ms) = (RevParameter i (BS.concat [addr, a]) b):(modMsg ms)
--         modMsg (m:ms) = m:(modMsg ms)
--         modMsg [] = []
--     modify $ unionR . T.fromList . fmap (\(a, ms) -> (BS.concat [addr, a], modMsg ms)) . T.toList $ tr
--     return addr
-- parseTree pre (BComp p f a) = do addr <- opsMessages pre p
--                                  aaddr <- parseTree pre a
--                                  let inNode = inOp
--                                      outNode = outOp
--                                  tr <- execStateT (parseTree pre $ outNode $ f inNode) T.empty
--                                  let modMsg ((Connect i a):ms) = (Connect i (BS.concat [addr, a])):(modMsg ms)
--                                      modMsg (m:ms) = m:(modMsg ms)
--                                      modMsg [] = []
--                                  modify $ unionR . T.fromList . fmap (\(a, ms) -> (BS.concat [addr, a], modMsg ms)) . T.toList $ tr
--                                  modify $ T.adjust ((:) (Connect 0 aaddr)) addr
--                                  return addr
parseTree pre (Comp p params ain bin cin din) = 
  do 
    addr <- opsMessages pre p
    aaddrs <- sequence $ parseTree pre <$> ain
    baddrs <- sequence $ parseTree pre <$> bin
    caddrs <- sequence $ parseTree pre <$> cin
    daddrs <- sequence $ parseTree pre <$> din
    mapM_ (\(k, p) -> 
      do 
        val <- parseParam p
        let msg = CustomPar k val
        modify $ T.adjust ((:) msg) addr
        return ()) (params)
    let 
      alladdrs = mconcat [aaddrs, baddrs, caddrs,daddrs]
      connects = L.zipWith Connect [0..] alladdrs
    modify $ T.adjust ((++) connects) addr 
    return addr
parseTree pre (FC fbnod reset loop sel) = 
  do 
    faddr <- parseTree pre $ N $ fbnod & chopIns .~ [reset]
    let fname = BS.tail faddr
    laddr <- parseTree (BS.concat [pre, "_", fname]) (loop $ fix fname $ selectCHOP id [])
    let lname = BS.tail laddr
    saddr <- parseTree (BS.concat [pre, "_", fname]) $ sel $ selectCHOP (selectCHOPchop ?~ (fix lname $ selectCHOP id [])) []
    let sname = BS.tail saddr
    modify $ T.adjust ((:) (RevConnect 0 faddr)) saddr
    removeDuplicates saddr
    return laddr
parseTree pre (FT fbnod reset loop sel) = 
  do 
    faddr <- parseTree pre $ N $ fbnod & topIns .~ [reset]
    let fname = BS.tail faddr
    laddr <- parseTree (BS.concat [pre, "_", fname]) (loop $ fix fname $ selectTOP id)
    let lname = BS.tail laddr
    saddr <- parseTree (BS.concat [pre, "_", fname]) $ sel $ selectTOP (selectTOPtop ?~ (fix lname $ selectTOP id))
    let sname = BS.tail saddr
    modify $ T.adjust ((:) (RevParameter "top" faddr laddr)) saddr
    removeDuplicates saddr
    return laddr
parseTree pre (Fix name op) = 
  do 
    messages <- get
    let name' = BS.append "/" name
    case T.member name' messages of
      True -> return name'
      False -> do 
        modify $ T.insert name' [(Fixed name)]
        addr <- parseTree pre op
        messages' <- get
        modify $ T.insert name' . ((:) (Fixed name)) . fromJust $ T.lookup addr messages'
        modify $ T.delete addr
        return name'

parseTree pre (PyExpr s) = pure s
parseTree pre (Mod f ta) = do aaddr <- parseParam ta
                              return . f $ aaddr
parseTree pre (Mod2 f ta tb) = do aaddr <- parseParam ta
                                  baddr <- parseParam tb
                                  return $ f aaddr baddr
parseTree pre (Mod3 f ta tb tc) = do aaddr <- parseParam ta
                                     baddr <- parseParam tb
                                     caddr <- parseParam tc
                                     return $ f aaddr baddr caddr
parseTree pre (Resolve r) = parseTree pre r
parseTree pre (ResolvePS l) = do ps <- sequence $ parseParam . ResolveP <$> l
                                 return $ BS.concat ["[", BS.intercalate "," ps, "]"]
parseTree pre (ResolveP r) = parseParam r

parseCommand :: (Monad m) => BS.ByteString -> CommandType -> StateT Messages m Messagable
parseCommand pre (Pulse bs v f) = pure $ Command "pulse" [bs, v, BS.pack $ show f]
parseCommand pre (Store bs t) = do ttype <- parseParam t
                                   return $ Command "store" [bs, ttype]


opsMessages :: (Monad m, Op a) => BS.ByteString -> a -> StateT Messages m BS.ByteString
opsMessages pre a = do let ty = opType a
                       messages <- get
                       let addr = findEmpty ty pre messages
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
                       mapM_ (\(i, op) -> do a <- parseTree pre op
                                             let connect = Connect i a
                                             modify $ T.adjust ((:) connect) addr
                                             return a) . Prelude.zip [0..] $ connections a
                       mapM_ (\c -> do m <- parseCommand pre c
                                       modify $ T.adjust ((:) m) addr
                                       return ()) (commands a)
                       addr' <- removeDuplicates addr
                       return $ addr'

removeDuplicates :: (Monad m) => BS.ByteString -> StateT Messages m BS.ByteString
removeDuplicates addr = do messages <- get
                           let nodesOfType = submap (BS.takeWhile (/= '_') addr) messages
                           let addrMsgs = T.lookup addr messages
                           -- If messages are all the same then they're equivalent so we can combine the nodes
                           case L.filter (\(a, ms) -> a /= addr && addrMsgs == Just ms) (T.toList nodesOfType) of
                             ((maddr, _):_) -> do modify . T.delete $ addr
                                                  return maddr
                             _ -> return addr

findEmpty :: BS.ByteString -> BS.ByteString -> Messages -> BS.ByteString
findEmpty ty pre ms = L.head . L.filter (not . flip T.member ms) . L.map (\n -> BS.concat ["/", ty, "_", BS.pack $ show n, pre]) $ [0..]

applyRevPars :: Messages -> Messages
applyRevPars ms = L.foldl (\ms (a, msgs) -> parseMessages ms a msgs) ms $ T.toList ms
  where
    parseMessages ms addr ((RevParameter par dest local):msgs) = T.adjust ((:) (Parameter par (wrapOp local))) dest ms
    parseMessages ms addr ((RevConnect par dest):msgs) = T.adjust (addConnect par addr) dest ms
    parseMessages ms addr (_:msgs) = parseMessages ms addr msgs
    parseMessages ms addr [] = ms
    addConnect i addr (cn@(Connect i' addr'):msgs) = (if i' >= i then Connect (i' + 1) addr' else cn):(addConnect i addr msgs)
    addConnect i addr (msg:msgs) = msg:(addConnect i addr msgs)
    addConnect i addr [] = [Connect i addr]

makeMessages :: Messages -> BS.ByteString
makeMessages msgs = BSL.toStrict . A.encode $ A.toJSON . A.object $
                    L.map (\(k, v) -> decodeUtf8 k A..= jsonNode v) $ T.toList $ applyRevPars msgs

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