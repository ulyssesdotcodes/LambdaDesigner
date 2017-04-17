module Lib
    ( someFunc
    , run
    ) where

import Op
import OSC

import Control.Monad.Trans.State
import Control.Lens
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD

someFunc :: IO ()
someFunc = do
  let tree = displace (movieFileIn "app.samplesFolder+'/Map/Jellybeans.1.jpg'") (movieFileIn "app.samplesFolder+'/Map/Jellybeans.1.jpg'")
  run tree

run :: Tree a -> IO ()
run tree = do
  conn <- openUDP "127.0.0.1" 9002
  ms <- execStateT (parseTree tree) empty
  let msgs = makeMessages ms
  mapM_ print msgs
  sendMessages conn msgs
  close conn
