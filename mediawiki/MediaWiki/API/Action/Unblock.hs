--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Unblock
-- Description : Representing Unblock requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Unblock requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Unblock where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data UnblockRequest
 = UnblockRequest
    { ublkId    :: Maybe String
    , ublkUser   :: Maybe UserName
    , ublkToken  :: Maybe Token
    , ublkGetToken :: Bool
    , ublkReason :: Maybe String
    }

instance APIRequest UnblockRequest where
  isPostable _ = True
  showReq r = 
   [ mbOpt "id" id (ublkId r)
   , mbOpt "user" id (ublkUser r)
   , mbOpt "token" id (ublkToken r)
   , optB "gettoken" (ublkGetToken r)
   , mbOpt "reason" id (ublkReason r)
   ]

emptyUnblockRequest :: UnblockRequest
emptyUnblockRequest = UnblockRequest
    { ublkId = Nothing
    , ublkUser   = Nothing
    , ublkToken  = Nothing
    , ublkGetToken = False
    , ublkReason = Nothing
    }

