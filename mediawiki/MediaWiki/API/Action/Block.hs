--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Block
-- Description : Representing Block requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Block requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Block where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data BlockRequest
 = BlockRequest
    { blkUser   :: UserName
    , blkToken  :: Token
    , blkGetToken :: Bool
    , blkExpiry :: Maybe Timestamp
    , blkReason :: Maybe String
    , blkAnonOnly :: Bool
    , blkNoCreate :: Bool
    , blkAutoBlock :: Bool
    , blkNoEmail :: Bool
    , blkHide    :: Bool
    }

instance APIRequest BlockRequest where
  isPostable _ = True
  showReq r =
    [ opt "user" (blkUser r)
    , opt "token" (blkToken r)
    , optB "gettoken" (blkGetToken r)
    , mbOpt "expiry" id (blkExpiry r)
    , mbOpt "reason" id (blkReason r)
    , optB "anononly" (blkAnonOnly r)
    , optB "nocreate" (blkNoCreate r)
    , optB "autoblock" (blkAutoBlock r)
    , optB "noemail"   (blkNoEmail r)
    , optB "hidename"  (blkHide r)
    ]


emptyBlockRequest :: BlockRequest
emptyBlockRequest = BlockRequest
    { blkUser   = ""
    , blkToken  = ""
    , blkGetToken = False
    , blkExpiry = Nothing
    , blkReason = Nothing
    , blkAnonOnly = False
    , blkNoCreate = False
    , blkAutoBlock = False
    , blkNoEmail = False
    , blkHide    = False
    }

