--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.UserInfo
-- Description : Representing 'userinfo' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'userinfo' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.UserInfo where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data UserInfoRequest
 = UserInfoRequest
    { uiProp       :: [String] 
       -- one of (1.13): 
       --      blockinfo, hasmsg, groups, rights, options, editcount, ratelimits
    }

instance APIRequest UserInfoRequest where
  queryKind _ = QMeta "userinfo"
  showReq r
   = [ opt1 "uiprop" (uiProp r)
     ]

emptyUserInfoRequest :: UserInfoRequest
emptyUserInfoRequest = UserInfoRequest
    { uiProp       = []
    }

data UserInfoResponse
 = UserInfoResponse
    { uiUser :: UserInfo
    }

emptyUserInfoResponse :: UserInfoResponse
emptyUserInfoResponse = UserInfoResponse
    { uiUser = emptyUserInfo
    }
