{-# LANGUAGE OverloadedStrings #-}

module LambdaDesigner.Lib
    ( topCompiler
    , compile
    , printMessages
    ) where

import LambdaDesigner.JSONOutput
import LambdaDesigner.Op
import LambdaDesigner.ParsedOps

import Control.Monad.Trans.State
import Data.ByteString.Char8 as BS
import Data.IORef
import Data.Functor.Identity
import Data.List
import Data.Trie

compile :: (Op a, Op b) => [Tree a] -> [Tree b] -> Messages -> Messages
compile tas tbs state' = let
    ms = execState (mapM_ (\t -> parseTree "" t) tas) mempty
    ms' = execState (mapM_ (\t -> parseTree "" t) tbs) ms
  in 
    unionR state' ms'

printMessages :: Messages -> BS.ByteString
printMessages state = makeMessages state

topCompiler :: IO (Tree TOP -> BS.ByteString)
topCompiler = do init <- newIORef mempty
                 initState <- readIORef init
                 return $ printMessages . flip (compile ([] :: [Tree TOP])) initState . (:[]) . outTOP id