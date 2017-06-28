module LocalNode (start) where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

start :: IO ()
start = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node    <- newLocalNode t initRemoteTable
  _       <- runProcess node $ do
    -- get our own process id
    self  <- getSelfPid
    send self "hello"
    hello <- expect :: Process String
    liftIO $ putStrLn hello
  return ()
