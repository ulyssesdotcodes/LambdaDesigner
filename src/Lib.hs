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

run :: (Op a) => Tree a -> IO ()
run tree = do
  conn <- openUDP "127.0.0.1" 9002
  ms <- execStateT (parseTree tree) empty
  let msgs = makeMessages ms
  mapM_ print msgs
  sendMessages conn msgs
  close conn
