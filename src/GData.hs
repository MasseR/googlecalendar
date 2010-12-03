module GData where

import Auth (Authentication, auth)

import Data.IORef (newIORef, readIORef)
import Data.URLEncoded ((%=))
import Control.Monad.Reader
import Network.Curl

type GData a = ReaderT Authentication IO a

testurl = "https://www.google.com/calendar/feeds/default/allcalendars/full"
getPage :: String -> GData String
getPage u =
  let url = u ++ "?" ++ show ("alt" %= "jsonc")
  in do
    header <- headers
    liftIO $ initialize >>= \h -> do
      ref <- newIORef []
      setopt h $ CurlHttpHeaders header
      setDefaultSSLOpts h url
      setopt h $ CurlFollowLocation True
      setopt h $ CurlURL url
      setopt h $ CurlWriteFunction $ gatherOutput ref
      rc <- perform h
      body <- readIORef ref
      return $ concat $ reverse body

-- Authorization: GoogleLogin auth=token
headers :: GData [String]
headers = ask >>= \a -> return [
    "Authorization:  GoogleLogin auth="++a
  , "GData-Version: 2"]

test :: GData ()
test = getPage testurl >>= liftIO . print

withAuthentication :: GData a -> IO a
withAuthentication f = do
  a <- auth
  case a of
       Left e -> error e
       Right authentication -> runReaderT f authentication
