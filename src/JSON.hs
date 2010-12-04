module JSON (
    CalendarList(..)
  , Data(..)
  , Author(..)
  , Item(..)
  )
where

{- XXX: Current state
- The parsing pretty much works, but only the Author has been checked
- for possible missing objects. Trying to render the test file fails
- miserably with missing items
-}

import Text.JSON (
    makeObj
  , JSON(..)
  , JSObject(..)
  , JSValue(..)
  , Result(..)
  , fromJSObject
  )
import Data.Map (Map)
import qualified Data.Map as M (lookup, fromList)
import Data.Maybe (fromJust)
import Control.Applicative ((<*>), (<$>))

data CalendarList = CalendarList {
    apiVersion :: String
  , cdata :: Data } deriving Show
instance JSON CalendarList where
  showJSON = makeObj . joinWithData' [
      ("apiVersion", showJSON . apiVersion)
    , ("data", showJSON . cdata)]
  readJSON (JSObject o) =
    let assoc = M.fromList $ fromJSObject o
    in CalendarList <$> is assoc "apiVersion" <*> is assoc "data"
data Data = Data {
    dkind :: String
  , detag :: String
  , did' :: String
  , dupdated :: String
  , dauthor :: Author 
  , dfeedLink :: String
  , dselfLink :: String
  , dcanPost :: Bool
  , ditems :: [Item] } deriving Show
instance JSON Data where
  showJSON = makeObj . joinWithData' [
      ("kind", showJSON . dkind)
    , ("etag", showJSON . detag)
    , ("id", showJSON . did')
    , ("updated", showJSON . dupdated)
    , ("author", showJSON . dauthor)
    , ("feedLink", showJSON . dfeedLink)
    , ("selfLink", showJSON . dselfLink)
    , ("canPost", showJSON . dcanPost)
    , ("items", showJSON . ditems)]
  readJSON (JSObject o) =
    let assoc = M.fromList $ fromJSObject o
    in Data
      <$> is assoc "kind"
      <*> is assoc "etag"
      <*> is assoc "id"
      <*> is assoc "updated"
      <*> is assoc "author"
      <*> is assoc "feedLink"
      <*> is assoc "selfLink"
      <*> is assoc "canPost"
      <*> is assoc "items"
data Author = Author {
    displayName :: String
  , email :: Maybe String } 
  deriving Show
instance JSON Author where
  readJSON (JSObject o) =
    let assoc = M.fromList $ fromJSObject o
    in Author <$> is assoc "displayName" <*> mightbe assoc "email"

  showJSON (Author d e) = makeObj $ [ ("displayName", showJSON d)]
    ?+ [("email", e)]
data Item = Item {
    ikind :: String
  , ietag :: String
  , eid' :: String
  , created :: String -- timestamp
  , iupdated :: String -- timestamp
  , title :: String
  , details :: Maybe String
  , eventFeedLink :: String
  , accessControlListLink :: Maybe String
  , selfLink :: String
  , canEdit :: Bool
  , author :: Author
  , accessLevel :: String
  , color :: String
  , hidden :: Bool
  , selected :: Bool
  , timeZone :: String
  , location :: Maybe String
  , timesCleaned :: Int} deriving Show
instance JSON Item where
  showJSON d = makeObj $ joinWithData' [
      ("kind", showJSON . ikind)
    , ("etag", showJSON . ietag)
    , ("id", showJSON . eid')
    , ("created", showJSON . created)
    , ("updated", showJSON . iupdated)
    , ("title", showJSON . title)
    , ("eventFeedLink", showJSON . eventFeedLink)
    , ("selfLink", showJSON . selfLink)
    , ("canEdit", showJSON . canEdit)
    , ("author", showJSON . author)
    , ("accessLevel", showJSON . accessLevel)
    , ("color", showJSON . color)
    , ("hidden", showJSON . hidden)
    , ("selected", showJSON . selected)
    , ("timeZone", showJSON . timeZone)
    , ("timesCleaned", showJSON . timesCleaned)
    ] d ?+ [
      ("details", details d)
    , ("accessControlListLink", accessControlListLink d)
    , ("location", location d)
    ]
  readJSON (JSObject o) =
    let assoc = M.fromList $ fromJSObject o
    in Item
      <$> is assoc "kind"
      <*> is assoc "etag"
      <*> is assoc "id"
      <*> is assoc "created"
      <*> is assoc "updated"
      <*> is assoc "title"
      <*> mightbe assoc "details"
      <*> is assoc "eventFeedLink"
      <*> mightbe assoc "accessControlListLink"
      <*> is assoc "selfLink"
      <*> is assoc "canEdit"
      <*> is assoc "author"
      <*> is assoc "accessLevel"
      <*> is assoc "color"
      <*> is assoc "hidden"
      <*> is assoc "selected"
      <*> is assoc "timeZone"
      <*> mightbe assoc "location"
      <*> is assoc "timesCleaned"

{- Helper methods -}

joinWithData' xs d = map (\(k, x) -> (k, x d)) xs

mlookup :: (JSON a, Ord k) => Map k JSValue -> k -> Maybe (Result a)
mlookup assoc key = readJSON `fmap` M.lookup key assoc

{- Maybe append -}
xs ?+ ((key, Just a):ys) = xs ++ [(key, showJSON a)] ?+ ys
xs ?+ ((key, Nothing):ys) = xs ?+ ys
xs ?+ [] = xs

-- We should have a value, and if there is none, it can be considered
-- as an error
is assoc key = fromJust $ mlookup assoc key
-- There might or might not be an object. Return Nothing if there
-- isn't
mightbe assoc key = mightbe' $ mlookup assoc key

-- If there are no results in the association table
-- return Nothing
mightbe' :: Maybe (Result a) -> Result (Maybe a)
mightbe' Nothing = Ok Nothing
mightbe' (Just a) = Just `fmap` a


