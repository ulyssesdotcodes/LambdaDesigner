module Lib
    ( topRunner
    , run
    , run2
    ) where

import Op
import OSC

import Control.Monad.Trans.State
import Data.IORef
import Data.List
import Data.Trie
import Sound.OSC.Transport.FD

import qualified Sound.OSC as OSC

run :: (Op a) => IORef Messages -> [Tree a] -> IO ()
run state tree = run2 state tree ([] :: [Tree TOP])

run2 :: (Op a, Op b) => IORef Messages -> [Tree a] -> [Tree b] -> IO ()
run2 state tas tbs = do
  state' <- readIORef state
  ms <- execStateT (mapM_ (\t -> do parseTree t) tas) mempty
  ms' <- execStateT (mapM_ (\t -> do parseTree t) tbs) ms
  let state'' = unionR state' ms'
      msgs = makeMessages state'
      msgs' = makeMessages state''
  writeIORef state state''
  conn <- OSC.openUDP "127.0.0.1" 9002
  sendMessages conn (msgs' \\ msgs)
  close conn
  return ()

topRunner :: IO (Tree TOP -> IO ())
topRunner = do init <- newIORef mempty
               return $ run init . (:[])
