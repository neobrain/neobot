--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Blocks
-- Description : Representing 'blocks' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'blocks' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Blocks where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data BlocksRequest
 = BlocksRequest
    { bkStart    :: Maybe Timestamp
    , bkEnd      :: Maybe Timestamp
    , bkDir      :: Maybe TimeArrow
    , bkIds      :: [UserID]
    , bkUsers    :: [UserID]
    , bkIp       :: [String]
    , bkLimit    :: Maybe Int
    , bkProp     :: [String]
    }

instance APIRequest BlocksRequest where
  queryKind _ = QList "blocks"
  showReq r
   = [ mbOpt "bkstart" id (bkStart r)
     , mbOpt "bkend"   id (bkEnd r)
     , mbOpt "bkdir"   (\ x -> if x==Earlier then "older" else "newer")
                       (bkDir r)
     , opt1 "bkids"    (bkIds r)
     , opt1 "bkusers"  (bkUsers r)
     , opt1 "bkip"     (bkIp r)
     , mbOpt "bklimit" show (bkLimit r)
     , opt1 "bkprop"    (bkProp r)
     ]

emptyBlocksRequest :: BlocksRequest
emptyBlocksRequest = BlocksRequest
    { bkStart    = Nothing
    , bkEnd      = Nothing
    , bkDir      = Nothing
    , bkIds      = []
    , bkUsers    = []
    , bkIp       = []
    , bkLimit    = Nothing
    , bkProp     = []
    }
    
data BlocksResponse
 = BlocksResponse
    { bkBlocks :: [BlockInfo]
    , bkContinue :: Maybe String
    }
    
emptyBlocksResponse :: BlocksResponse
emptyBlocksResponse = BlocksResponse
    { bkBlocks   = []
    , bkContinue = Nothing
    }

data BlockInfo
 = BlockInfo
    { bkId :: Maybe String
    , bkUser :: Maybe UserName
    , bkBy   :: Maybe UserName
    , bkTimestamp :: Maybe Timestamp
    , bkExpiry :: Maybe Timestamp
    , bkReason :: Maybe String
    , bkRangeStart :: Maybe String
    , bkRangeEnd :: Maybe String
    , bkIsAuto :: Bool
    , bkIsAnonOnly :: Bool
    , bkIsNoCreate :: Bool
    , bkIsAutoBlock :: Bool
    , bkIsNoEmail :: Bool
    , bkIsHidden :: Bool
    }
    
emptyBlockInfo :: BlockInfo
emptyBlockInfo = BlockInfo
    { bkId = Nothing
    , bkUser = Nothing
    , bkBy   = Nothing
    , bkTimestamp = Nothing
    , bkExpiry = Nothing
    , bkReason = Nothing
    , bkRangeStart = Nothing
    , bkRangeEnd = Nothing
    , bkIsAuto = False
    , bkIsAnonOnly  = False
    , bkIsNoCreate  = False
    , bkIsAutoBlock  = False
    , bkIsNoEmail  = False
    , bkIsHidden  = False
    }



