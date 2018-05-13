{-# LANGUAGE OverloadedStrings #-}

module LambdaDesigner.Lib
    ( topCompiler
    , compile
    ) where

import LambdaDesigner.JSONOutput
import LambdaDesigner.Op

import Control.Monad.Trans.State
import Data.ByteString.Char8 as BS
import Data.IORef
import Data.Functor.Identity
import Data.List
import Data.Trie

compile :: (Op a, Op b) => [Tree a] -> [Tree b] -> Messages -> BS.ByteString
compile tas tbs state' = let
    ms = execState (mapM_ (\t -> parseTree "" t) tas) mempty
    ms' = execState (mapM_ (\t -> parseTree "" t) tbs) ms
    state'' = unionR state' ms'
    msgs = makeMessages state'
  in 
    makeMessages ms'

topCompiler :: IO (Tree TOP -> BS.ByteString)
topCompiler = do init <- newIORef mempty
                 initState <- readIORef init
                 return $ flip (compile ([] :: [Tree TOP])) initState . (:[]) . outT