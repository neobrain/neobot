--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllUsers
-- Description : Representing 'allusers' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'allusers' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllUsers where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data AllUsersRequest
 = AllUsersRequest
    { auFrom   :: Maybe UserName
    , auPrefix :: Maybe UserName
    , auGroup  :: Maybe GroupName
    , auProp   :: [String]
    , auLimit  :: Maybe Int
    }
    
instance APIRequest AllUsersRequest where
  queryKind _ = QList "allusers"
  showReq r = 
   [ mbOpt "aufrom" id (auFrom r)
   , mbOpt "auprefix" id (auPrefix r)
   , mbOpt "augroup" id (auGroup r)
   , opt1  "auprop" (auProp r)
   , mbOpt "aulimit" show (auLimit r)
   ]
    

emptyAllUsersRequest :: AllUsersRequest
emptyAllUsersRequest = AllUsersRequest
    { auFrom   = Nothing
    , auPrefix = Nothing
    , auGroup  = Nothing
    , auProp   = []
    , auLimit  = Nothing
    }

data AllUsersResponse
 = AllUsersResponse
    { auUsers    :: [(UserName, Maybe Int, Maybe String)]
    , auContinue :: Maybe String
    }
    
emptyAllUsersResponse :: AllUsersResponse
emptyAllUsersResponse
 = AllUsersResponse
    { auUsers    = []
    , auContinue = Nothing
    }
