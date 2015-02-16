{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Data.Aeson
import System.Process
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Monad.Reader (lift)
import Data.List.Split as L
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
	liftIO $ putStrLn $ case m of
		"GET" -> "User requested with GET" $ do 
			lines <- liftIO $ readFile $ mconcat [cgroupDirectory, l, "/tasks"]
			let splitLines = splitOn "\n" lines
			liftIO $ print splitLines
			respond $ index splitLines

main = do
	let port = 3000
	putStrLn $ "Listening on " ++ show port
	run port app


--begin port a = run port a


index x = responseLBS status200 [(hContentType, "application/json")] (encode x) 
