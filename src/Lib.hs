{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runInfoRequest, cryptSign, getInfoValues, test, hexTest, getTime
    ) where

import Control.Monad.Loops(iterateUntilM)
import Control.Concurrent(threadDelay)
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Char8
--import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Hash.SHA512 as SHA512
import Network.HTTP.Base
--import Network.HTTP.Client
import Network.Wreq
import Data.Time.Clock.POSIX
import Control.Lens
import Data.Aeson.Types
import Data.Aeson
import Data.ByteString.UTF8
import Data.Hex
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8, decodeUtf8)

initWorld :: Int
initWorld = 1

-- False when the user wants to exit the game
keepGoing :: Int -> Int -> Bool
keepGoing m w = (w >= m)

displayWorld :: Int -> IO ()
displayWorld i = do
  print i
  
             
gameLoop :: Int -> Int
gameLoop = (+1)

someFunc :: Int -> String -> String -> IO Int
someFunc c key secret = iterateUntilM (keepGoing c) displayLoop initWorld
   where displayLoop w = do
           threadDelay 1000000 -- microseconds not millis!
           displayWorld w >> return (gameLoop w) 

cryptSign :: String -> String -> ByteString
--sign secret values = (hmac SHA512.hash 128 (secret) ({-Data.ByteString.Char8.pack-} values))

cryptSign secret values = (SHA512.hmac (Data.ByteString.Char8.pack secret) (Data.ByteString.Char8.pack values))

getInfoValues :: Int -> IO String  
getInfoValues t = do
  return (urlEncodeVars [("nonce", show t ), ("method", "getInfo")])

getTime:: IO Int
getTime = round `fmap` getPOSIXTime   

test :: Int -> String -> IO ByteString
test t secret = do
  --t <- round `fmap` getPOSIXTime 
  v <- getInfoValues t
  return (cryptSign secret v)  

hexTest :: ByteString -> ByteString
hexTest inStr = hex inStr

runInfoRequest :: String -> String -> IO (Network.Wreq.Response LC.ByteString)
runInfoRequest key secret = do
  t <- round `fmap` getPOSIXTime 
  let st = T.pack (show t)
--  let st = show t
  v <- getInfoValues t
  let sign = T.toLower (decodeUtf8 (hex (cryptSign secret v)))
  let opts = defaults & header "Content-Type" .~ [Data.ByteString.Char8.pack  "application/x-www-form-urlencoded"] & header "Key" .~ [Data.ByteString.Char8.pack key] & header "Sign" .~ [encodeUtf8 sign] & params .~ [("nonce", st),("method","getInfo")]
  print opts
  r <- postWith opts "https://yobit.net/tapi/" [(Data.ByteString.Char8.pack "nonce") := show t, (Data.ByteString.Char8.pack "method") := (Data.ByteString.Char8.pack "getInfo")]
--[(Data.ByteString.Char8.pack "1") := (Data.ByteString.Char8.pack "1")]
  return r

--getCurrentPrice :: String -> Bool -> Double
--request pair pricetype = 

