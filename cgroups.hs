{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import System.Process
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Lens 
import Data.List.Split
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Data.Monoid

cgroupDirectory :: String
cgroupDirectory = "/sys/fs/cgroup"

subsystems = ["blkio", "cpu", "cpuacct", "cpuset", "devices", "freezer", "memory", "net_cls", "net_prio", "ns"]



get pathInfo respond = do 
       lines <- liftIO $ readFile $ mconcat [cgroupDirectory, pathInfo, "/tasks"]
       let splitLines = splitOn "\n" lines
       liftIO $ print splitLines
       respond $ htmlResponse splitLines

--getElementAt :: a -> b -> MaybeT IO String
--getElementAt pathList index :: splitOn "/" pathList
classify :: String -> String -> String -> IO String
classify a b c = readProcess "cgclassify" ["-g", a  <> ":" <> b, c] []

extract :: String -> IO String
extract pathInfo = do
       let pathList = splitOn "/" pathInfo
       let maybeController = pathList ^? element 1
       let cgroup = pathList ^? element 2
       let pid = pathList ^? element 3
       case maybeController of
         Just a -> case cgroup of
           Just b -> case pid of
             Just c -> (classify a b c)
             Nothing -> return "No Process ID"
           Nothing -> return "No cgroup"
         Nothing -> return "No controller"


put pathInfo respond = do
	result <- liftIO $ extract pathInfo
        respond ( htmlResponse result)

post pathInfo respond = do
       let split = splitOn "/" pathInfo
       let controller = split !! 1
       let cgroup = split !! 2
       response <- liftIO $ readProcess "cgcreate" ["-g", controller <> ":" <> cgroup] []
       putStrLn $ "Creating cgroup with controller " <> controller <> " and cgroup name " <> cgroup
       let s = "hi" :: String
       respond $ htmlResponse s

app req respond = do
	let requestPath = (BS.unpack . rawPathInfo) req 
	let m = (BS.unpack . requestMethod) req
	liftIO $ putStrLn requestPath
	liftIO $ case m of
		"GET" -> get requestPath respond
		"PUT"  -> put requestPath respond 
		"POST" -> post requestPath respond

main = do
	let port = 3000
	putStrLn $ "Listening on " <> show port
	run port app


htmlResponse x = responseLBS status200 [(hContentType, "application/json")] (encode x) 
