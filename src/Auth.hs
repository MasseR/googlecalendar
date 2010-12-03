module Auth (auth, Authentication) where

import Network.Curl
import Data.URLEncoded ((%=))
import Data.IORef (newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.ConfigFile
import Control.Monad.Error
import System.Directory


url = "https://www.google.com/accounts/ClientLogin"

type Authentication = String
data Credentials = Credentials {
    email :: String
  , passwd :: String
  , source :: String
  , service :: String
  } deriving Show

gcal :: Credentials
gcal = Credentials "" "" "exampleCo-exampleApp-1" "cl"

defaultConfPath = "config"

readCred :: FilePath -> IO (Either CPError Credentials)
readCred f = runErrorT $ do
  exists <- liftIO $ doesFileExist f
  if exists
    then do
      cp <- join $ liftIO $ readfile emptyCP f
      e <- get cp "DEFAULT" "email"
      p <- get cp "DEFAULT" "password"
      return $ gcal {email = e, passwd = p}
    else throwError (OtherProblem "No such file", f)

cred :: Credentials -> [String]
cred (Credentials e p so se) = map show [
    "Email" %= e
  , "Passwd" %= p
  , "source" %= so
  , "service" %= se]

readAuth :: [String] -> Either String Authentication
readAuth xs | length xs < 3 = Left $ concat xs
	    | otherwise =
	    let a = filter ("Auth" `isPrefixOf`) xs
	    in case a of
		    [] -> Left "No parse"
		    [x] -> Right $ tail $ dropWhile (/= '=') x

auth' :: Credentials -> IO (Either String Authentication)
auth' c = initialize >>= \h -> do
  ref <- newIORef []
  setopt h $ CurlPost True
  setopt h $ CurlPostFields $ cred c
  setopt h $ CurlURL url
  setopt h $ CurlWriteFunction $ gatherOutput ref
  setDefaultSSLOpts h url
  rc <- perform h
  body <- readIORef ref
  return $ readAuth $ lines $ concat body

auth :: IO (Either String Authentication)
auth = do
  c <- readCred defaultConfPath
  case c of
       Left (e, s) -> return $ Left $ show e ++ ":" ++ s
       Right credentials -> auth' credentials

