--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Utils
-- Description : MediaWiki API internal utility functions.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- MediaWiki API internal utility functions.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Utils 
       ( module MediaWiki.API.Utils
       , fromMaybe
       ) where

import Text.XML.Light as XML
import Data.Maybe
import Data.List
import Control.Monad

import MediaWiki.Util.Codec.URLEncoder ( encodeString )

pNodes       :: String -> [XML.Element] -> [XML.Element]
pNodes x es   = filter ((nsName x ==) . elName) es

pNode        :: String -> [XML.Element] -> Maybe XML.Element
pNode x es    = listToMaybe (pNodes x es)

pLeaf        :: String -> [XML.Element] -> Maybe String
pLeaf x es    = strContent `fmap` pNode x es

pAttr        :: String -> XML.Element -> Maybe String
pAttr x e     = lookup (nsName x) [ (k,v) | Attr k v <- elAttribs e ]

pMany        :: String -> (XML.Element -> Maybe a) -> [XML.Element] -> [a]
pMany p f es  = mapMaybe f (pNodes p es)

children     :: XML.Element -> [XML.Element]
children e    = onlyElems (elContent e)

nsName :: String -> QName
nsName x = QName{qName=x,qURI=Nothing,qPrefix=Nothing}

without :: [String] -> [XML.Attr] -> [XML.Attr]
without xs as = filter (\ a -> not (attrKey a `elem` qxs)) as
 where
  qxs = map nsName xs

parseDoc :: (Element -> Maybe a) -> String -> Either (String,[{-error msg-}String]) a
parseDoc f s = 
  case parseXMLDoc s of
    Nothing -> Left (s, ["not valid XML content"])
    Just d  ->
      case f d of
        Nothing -> Left (s,["unexpected XML response"])
	Just x  -> Right x

xmlContinue :: String -> String -> Element -> Maybe String
xmlContinue tgName atName e = do
  guard (elName e == nsName "query-continue")
  let es1 = children e
  p  <- pNode tgName es1
  pAttr atName p

mbDef :: a -> Maybe a -> Maybe a
mbDef x Nothing = Just x
mbDef _ v = v

readMb :: Read a => String -> Maybe a
readMb x = case reads x of
             ((v,_):_) -> Just v
	     _ -> Nothing

piped :: [String] -> String
piped xs = intercalate "|" xs

opt :: String -> String -> Maybe (String,String)
opt a b = Just (a, encodeString b)

optB :: String -> Bool -> Maybe (String,String)
optB _ False = Nothing
optB a _ = Just (a,"")

opt1 :: String -> [String] -> Maybe (String,String)
opt1 _ [] = Nothing
opt1 a b = Just (a,encodeString $ piped b)

mbOpt :: String -> (a -> String) -> Maybe a -> Maybe (String,String)
mbOpt  _ _ Nothing = Nothing
mbOpt tg f (Just x) = Just (tg,encodeString $ f x)
