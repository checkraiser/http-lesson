module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Data.Aeson
import Data.Text as T 
import GHC.Generics
import Control.Monad (forM_)

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request 
buildRequest token host method path = setRequestMethod method 
                  $ setRequestHost host 
                  $ setRequestHeader "token" [token]
                  $ setRequestPath path 
                  $ setRequestSecure True 
                  $ setRequestPort 443
                  $ defaultRequest
request :: BC.ByteString -> Request 
request myToken = buildRequest myToken noaaHost "GET" apiPath                 
 
data ResultSet = ResultSet { offset :: Int, count :: Int, limit :: Int } deriving (Show, Generic)
instance FromJSON ResultSet
data Metadata = Metadata { resultset :: ResultSet } deriving (Show, Generic)
instance FromJSON Metadata
data NooaResult = NooaResult { uid :: T.Text, mindate :: T.Text, maxdate :: T.Text, name :: T.Text, datacoverage :: Double, resultId :: T.Text } deriving (Show)
instance FromJSON NooaResult where 
  parseJSON (Object v) =
    NooaResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"
data NooaResponse = NooaResponse { metadata :: Metadata, results :: [NooaResult] } deriving (Show, Generic)
instance FromJSON NooaResponse

printNooaResuls :: Maybe [NooaResult] -> IO ()
printNooaResuls Nothing = print "error loading data"
printNooaResuls (Just results) = do 
  print results
  forM_ results $ \result -> do 
    let dataName = name result 
    print dataName

main :: IO ()
main = do
  print "Please input token:"
  -- ask token
  token <- BC.getLine 
  -- get response
  response <- httpLBS $ request token
  -- check status code
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      --L.writeFile "data.json" jsonBody
      -- read data then print result content
      --jsonData <- B.readFile "data.json"
      let nooaResponse = decode jsonBody :: Maybe NooaResponse
      let nooaResults = results <$> nooaResponse
      printNooaResuls nooaResults
  -- if 200 then write json body to file
  else print "request failed"
  -- else print fail
