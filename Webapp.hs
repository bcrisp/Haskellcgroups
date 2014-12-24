{-# LANGUAGE OverloadedStrings #-}

module Webapp where 

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
--import Blaze.ByteString.Builder (copyByteString)
--import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Control.Monad.IO.Class
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)


--main = do
--	let port = 3000
--	putStrLn $ "Listening on " ++ show port
--	run port app

begin port a = run port a

--app req respond = respond $
--	case pathInfo req of 
--	["yay"] -> yay
--	x -> index x

--yay = responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString ["yay"]


index x = responseLBS status200 [(hContentType, "application/json")] (encode x) -- $ mconcat $ map 
  --  [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
  --  , "<p><a href='/yay'>yay</a></p>\n" ]
--
--index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map ----copyByteString
--	["Hello from ", BU.fromString $ show x, "!"]
