{-# LANGUAGE TemplateHaskell #-}

module Main
where

import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import Control.Distributed.Process (say, Process, spawnLink, RemoteTable)
import Control.Distributed.Process.Node (newLocalNode, initRemoteTable, runProcess, localNodeId, LocalNode(..))
import Control.Distributed.Process.Internal.Types (NodeId(..))
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Concurrent (threadDelay)
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (defaultTCPParameters, createTransport)
import Control.Monad (replicateM_)
import Control.Monad.Trans  (liftIO)

sleepSeconds :: Int -> IO ()
sleepSeconds n = threadDelay (1000000 * n)

printLoop :: String -> Int -> Process ()
printLoop s n = replicateM_ n $ do
  say s
  liftIO $ sleepSeconds 1;

printProcess :: (String, Int) -> Process ()
printProcess (s, n) = printLoop s n

remotable ['printProcess]

startLocalNode :: String -> String -> IO LocalNode
startLocalNode host port = do
  Right transport <- createTransport host port defaultTCPParameters
  newLocalNode transport rtable

nodeFromEndpoint :: String -> NodeId
nodeFromEndpoint = NodeId . EndPointAddress . pack

leaderProcess :: LocalNode -> NodeId -> IO ()
leaderProcess localNode remoteNode =
  runProcess localNode go
    where
      go = do
        line <- liftIO getLine
        let [m, n] = words line
        liftIO $ print (m, n)
        _ <- spawnLink remoteNode ($(mkClosure 'printProcess) (m, read n :: Int))
        go

startLeader :: String -> String -> String -> IO ()
startLeader host port followerAddress = do
  localNode  <- startLocalNode host port
  let nodeId = nodeFromEndpoint followerAddress
  leaderProcess localNode nodeId

startFollower :: String -> String -> IO ()
startFollower host port = do
  localNode  <- startLocalNode host port
  print (localNodeId localNode)
  sleepSeconds 1000

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable

usage :: String
usage = concat ["First, start a follower:\n",
                 "./test follower localhost 20000\n",
                 "this will print out a node id, like: ",
                 "nid://localhost:20000:0\n",
                 "copy this id (minus the nid:// bit)\n",
                 "and start the leader:\n",
                 "./test leader localhost 5001 localhost:20000:0\n",
                 "Now type a message and a number of times for it to print:\n",
                 "foo 10\n"]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      putStr usage
    ["leader", host, port, followerAddress] ->
      startLeader host port followerAddress
    ["follower", host, port] ->
      startFollower host port
    _ ->
      putStr usage
