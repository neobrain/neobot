--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Login.Import
-- Description : Serializing Login requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Serializing Login requests.
--
--------------------------------------------------------------------
module MediaWiki.API.Action.Login.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Action.Login

import Text.XML.Light.Types

import Control.Monad

stringXml :: String -> Either (String,[{-Error msg-}String]) LoginResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe LoginResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p <- pNode "login" es1
  let res = pAttr "result" p
  let uid = pAttr "lguserid" p
  let unm = pAttr "lgusername" p
  let tok = pAttr "lgtoken" p
  let coo = pAttr "cookieprefix" p
  let ses = pAttr "sessionid" p
  case res of
    Nothing -> fail "missing 'result' api attribute"
    Just "Success" ->
       return LoginResponse{ lgSuccess=True
                           , lgSession=UserSession
			                 { sessUserId    = fromMaybe "" uid
					 , sessUserName  = fromMaybe "" unm
					 , sessPassword  = Nothing
					 , sessCookiePrefix = coo
					 , sessSessionId = ses
					 , sessToken     =  fromMaybe "" tok
					 }}
    Just x -> do
       let det = pAttr "details" p
       let wai = pAttr "wait" p
       return LoginError
                { lgSuccess = False
		, lgeError  = x
		, lgeDetails = det
		, lgeWait   = fromMaybe "" wai
		}

