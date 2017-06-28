{-# LANGUAGE TemplateHaskell #-}

module RemoteNode (start) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = RemoteNode.__remoteTable initRemoteTable

start :: IO ()
start = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node    <- newLocalNode t myRemoteTable
  runProcess node $ do
    us  <- getSelfNode
    _   <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
    pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
    liftIO $ threadDelay 2000000
