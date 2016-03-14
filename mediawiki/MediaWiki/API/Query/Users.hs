--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Users
-- Description : Representing 'users' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'users' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Users where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data UsersRequest
 = UsersRequest
    { usProp       :: [String]
    , usUsers      :: [UserID]
    }

instance APIRequest UsersRequest where
  queryKind _ = QList "users"
  showReq r = [ opt1 "usprop"  (usProp r)
              , opt1 "ususers" (usUsers r)
	      ]

emptyUsersRequest :: UsersRequest
emptyUsersRequest = UsersRequest
    { usProp       = []
    , usUsers      = []
    }

data UsersResponse
 = UsersResponse
    { usrUsers    :: [UsersInfo]
    }
    
emptyUsersResponse :: UsersResponse
emptyUsersResponse
 = UsersResponse
    { usrUsers    = []
    }

data UsersInfo
 = UsersInfo
    { usName :: Maybe UserName
    , usEditCount :: Maybe Int
    , usRegDate :: Maybe Timestamp
    , usGroups :: [String]
    , usBlock  :: Maybe (UserName,String)
    }
    
emptyUsersInfo :: UsersInfo
emptyUsersInfo = UsersInfo
    { usName = Nothing
    , usEditCount = Nothing
    , usRegDate = Nothing
    , usGroups = []
    , usBlock  = Nothing
    }
  
