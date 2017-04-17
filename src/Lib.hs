module Lib
    ( someFunc
    , run
    ) where

import Op
import OSC

import Control.Monad.Trans.State
import Data.Trie
import Sound.OSC
import Sound.OSC.Transport.FD

someFunc :: IO ()
someFunc = do
  let tree = displace (movieFileIn "app.samplesFolder+'/Map/Jellybeans.1.jpg'") (movieFileIn "app.samplesFolder+'/Map/Jellybeans.1.jpg'")
  conn <- openUDP "127.0.0.1" 9002
  (_, ms) <- evalStateT (parseTree tree) empty
  print $ ms
  sendMessages conn ms
  close conn

run :: Tree TOP -> IO ()
run tree = do
  conn <- openUDP "127.0.0.1" 9002
  (_, ms) <- evalStateT (parseTree tree) empty
  mapM_ print ms
  sendMessages conn ms
  close conn
