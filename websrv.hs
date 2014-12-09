-- 2014 Brandon Crisp

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Process
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as B
import Data.List.Split as L
import Control.Monad
import Control.Monad.Reader (lift)

import Data.Monoid (mconcat)

subsystems = ["blkio", "cpu", "cpuacct", "cpuset", "devices", "freezer", "memory", "net_cls", "net_prio", "ns"]

cgroupDirectory :: String
cgroupDirectory = "/sys/fs/cgroup/"

isContained :: Eq a => [a] -> [a] -> [a]
isContained list sublist = filter ( `notElem` list ) sublist

main = scotty 3000 $ do
	get "/:cgroup" $ do -- list pids
		cgroup <- param "cgroup"
		let path = cgroupDirectory ++ cgroup ++ "/tasks"
		response <- liftIO $ readFile path
		let pids = lines response
		json $ pids
	put "/cgroup/:name/:groups/:pid" $ do -- place process into cgroup
		name <- param "name"
		g <- param "groups"
		let groups = splitOn "," $ B.unpack g -- (param "groups")
		let invalidGroups = isContained subsystems groups
		if ((length(invalidGroups)) > 0) then
			raise $ mconcat ["The following subsystems are not valid: ", mconcat $ fmap B.pack invalidGroups]
		else do
			res <- liftIO $ readProcess "cgclassify" [] []
			html $ mconcat ["<h1>Creating cgroup with name ",  B.pack res, name]
	post "/cgroup/:name/:groups" $ do -- create a cgroup
		name <- param "name"
		g <- param "groups"
		let list = splitOn "," $ B.unpack g
		res <- liftIO $ readProcess "cgcreate" name []
		html $ mconcat ["<h1>Creating cgroup with name ",  B.pack $ head list, ":", B.pack res]