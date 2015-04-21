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
import Control.Applicative
import Control.Error.Util
import Data.Traversable

cgroupDirectory :: String
cgroupDirectory = "/sys/fs/cgroup"

subsystems = ["blkio", "cpu", "cpuacct", "cpuset", "devices", "freezer", "memory", "net_cls", "net_prio", "ns"]

get pathInfo respond = do 
       lines <- liftIO $ readFile $ mconcat [cgroupDirectory, pathInfo, "/tasks"]
       let splitLines = splitOn "\n" lines
       liftIO $ print splitLines
       respond $ htmlResponse splitLines

classify :: String -> String -> String -> IO String
classify a b c = readProcess "cgclassify" ["-g", a  <> ":" <> b, c] []

runP :: String -> String -> IO String
runP controller cgroup = readProcess "cgcreate" ["-g", controller <> ":" <> cgroup] []

extract :: String -> Either String (IO String)
extract pathInfo = do
       let pathList = splitOn "/" pathInfo
       let maybeController = note "Can't find controller" (pathList ^? element 1)
       let cgroup = note "Can't find cgroup" (pathList ^? element 2)
       let pid = note "Can't find pid" (pathList ^? element 3)
       classify <$> maybeController <*> cgroup <*> pid

put pathInfo respond = (sequenceA $ extract pathInfo) >>= respond . htmlResponse

post pathInfo respond = do
       let pathList = splitOn "/" pathInfo
       let maybeController = note "Can't find controller" (pathList ^? element 1) :: Either String String
       let cgroup = note "Can't find cgroup" (pathList ^? element 2)
       (respond . htmlResponse) =<< (sequenceA (runP <$> maybeController <*> cgroup))

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
