{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import System.Process
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.List.Split
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Data.Monoid (mconcat)

cgroupDirectory :: String
cgroupDirectory = "/sys/fs/cgroup"

subsystems = ["blkio", "cpu", "cpuacct", "cpuset", "devices", "freezer", "memory", "net_cls", "net_prio", "ns"]

app req respond = do
	let l = (BS.unpack . rawPathInfo) req 
	let m = (BS.unpack . requestMethod) req
	liftIO $ putStrLn l
	liftIO $ case m of
		"GET" -> do 
			lines <- liftIO $ readFile $ mconcat [cgroupDirectory, l, "/tasks"]
			let splitLines = splitOn "\n" lines
			liftIO $ print splitLines
			respond $ index splitLines
		"PUT"  -> do
		        let split = splitOn "/" l
                        let controller = split !! 1--  (intercalate "/" . init) split
	                --let b = liftM (isInfixOf cgroup) $ readProcess "lscgroup" [] []
			let cgroup = split !! 2
			let pid = last split
			response <- liftIO $ readProcess "cgclassify" ["-g",  controller ++ ":" ++ cgroup, pid] []
                        respond $ index $ response
		"POST" -> do
			let split = splitOn "/" l
			let controller = split !! 1
			let cgroup = split !! 2	
			response <- liftIO $ readProcess "cgcreate" ["-g", controller ++ ":" ++ cgroup] []
			putStrLn $ "Creating cgroup with controller " ++ controller ++ " and cgroup name " ++ cgroup
			let s = "hi" :: String
			respond $ index s

main = do
	let port = 3000
	putStrLn $ "Listening on " ++ show port
	run port app


index x = responseLBS status200 [(hContentType, "application/json")] (encode x) 
