module Lib
    ( run
    ) where

import Op
import OSC
import Tree

import Control.Monad.Trans.State
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD

run :: (Op a) => [Tree a] -> Messages -> IO Messages
run tree state = do
  conn <- openUDP "127.0.0.1" 9002
  ms <- execStateT (mapM_ (\t -> do parseTree t) tree) state
  let msgs = makeMessages ms
  mapM_ print msgs
  sendMessages conn msgs
  close conn
  return ms
