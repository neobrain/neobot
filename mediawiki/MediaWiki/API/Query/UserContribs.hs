--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.UserContribs
-- Description : Representing 'usercontribs' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'usercontribs' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.UserContribs where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data UserContribsRequest 
 = UserContribsRequest
    { ucLimit      :: Maybe Int
    , ucStart      :: Maybe Timestamp
    , ucEnd        :: Maybe Timestamp
    , ucUser       :: Maybe UserID
    , ucUserPrefix :: Maybe String
    , ucDir        :: Maybe TimeArrow
    , ucNamespaces :: [NamespaceID]
    , ucProp       :: [String]
    , ucShow       :: [String]
    }

instance APIRequest UserContribsRequest where
  queryKind _ = QList "usercontribs"
  showReq r
   = [ mbOpt "uclimit" show (ucLimit r)
     , mbOpt "ucstart" id   (ucStart r)
     , mbOpt "ucend"   id   (ucEnd r)
     , mbOpt "ucuser"  id   (ucUser r)
     , mbOpt "ucuserprefix" id (ucUserPrefix r)
     , mbOpt "ucdir" (\ x -> if x == Earlier then "older" else "newer") (ucDir r)
     , opt1  "ucnamespace" (ucNamespaces r)
     , opt1  "ucprop" (ucProp r)
     , opt1  "ucshow" (ucShow r)
     ]


emptyUserContribsRequest :: UserContribsRequest
emptyUserContribsRequest = UserContribsRequest
    { ucLimit      = Nothing
    , ucStart      = Nothing
    , ucEnd        = Nothing
    , ucUser       = Nothing
    , ucUserPrefix = Nothing
    , ucDir        = Nothing
    , ucNamespaces = []
    , ucProp       = []
    , ucShow       = []
    }

data UserContribsResponse
 = UserContribsResponse
    { ucPages    :: [UserContrib]
    , ucContinue :: Maybe String
    }

emptyUserContribsResponse :: UserContribsResponse
emptyUserContribsResponse = UserContribsResponse
    { ucPages    = []
    , ucContinue = Nothing
    }

data UserContrib
 = UserContrib
    { ucoUser      :: UserName
    , ucoPage      :: PageTitle
    , ucoRevId     :: RevID
    , ucoTimestamp :: Maybe Timestamp
    , ucoIsNew     :: Bool
    , ucoIsMinor   :: Bool
    , ucoIsTop     :: Bool
    , ucoComment   :: Maybe String
    }
    
emptyUserContrib :: UserContrib
emptyUserContrib  = UserContrib
    { ucoUser  = ""
    , ucoPage  = emptyPageTitle
    , ucoRevId = "0"
    , ucoTimestamp = Nothing
    , ucoIsNew     = False
    , ucoIsMinor   = False
    , ucoIsTop     = False
    , ucoComment   = Nothing
    }
    



    
