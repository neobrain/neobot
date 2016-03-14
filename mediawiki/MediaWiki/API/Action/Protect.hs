--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Protect
-- Description : Representing Protect requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Protect requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Protect where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ProtectRequest
 = ProtectRequest
    { protTitle  :: PageName
    , protToken  :: Token
    , protProtections :: [(String,String)]
    , protExpiry :: Maybe Timestamp
    , protReason :: Maybe String
    , protCascade :: Maybe Bool
    }

instance APIRequest ProtectRequest where
 isPostable _ = True
 showReq r =
  [ opt "title" (protTitle r)
  , opt "token" (protToken r)
  , opt1 "protections" (map (\ (a,b) -> a ++ '=':b) (protProtections r))
  , mbOpt "expiry" id (protExpiry r)
  , mbOpt "reason" id (protReason r)
  , optB "cascade" (fromMaybe False $ protCascade r)
  ]

emptyProtectRequest :: ProtectRequest
emptyProtectRequest = ProtectRequest
    { protTitle = ""
    , protToken = ""
    , protProtections = []
    , protExpiry = Nothing
    , protReason = Nothing
    , protCascade = Nothing
    } 

