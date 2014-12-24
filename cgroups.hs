{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Data.Aeson
import System.Process
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.Reader (lift)
import Webapp
import Data.List.Split as L

import Data.Monoid (mconcat)

cgroupDirectory :: String
cgroupDirectory = "/sys/fs/cgroup"

subsystems = ["blkio", "cpu", "cpuacct", "cpuset", "devices", "freezer", "memory", "net_cls", "net_prio", "ns"]

app req respond = do
	let l = BS.unpack $ rawPathInfo req 
	liftIO $ putStrLn l
	lines <- liftIO $ readFile $ mconcat [cgroupDirectory, l, "/tasks"]
	let splitLines = splitOn "\n" lines
	liftIO $ print splitLines
	respond $ index  splitLines

main = do
	let port = 3000
	putStrLn $ "Listening on " ++ show port
	begin port app
