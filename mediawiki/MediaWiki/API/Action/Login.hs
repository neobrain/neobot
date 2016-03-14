--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Login
-- Description : Representing Login requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Login requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Login where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data LoginRequest
 = LoginRequest
     { lgName     :: User
     , lgPassword :: Password
     , lgDomain   :: Maybe String
     }

instance APIRequest LoginRequest where
  isPostable _ = True
  showReq r =
   [ opt "lgname" (lgName r)
   , opt "lgpassword" (lgPassword r)
   , mbOpt "lgdomain" id (lgDomain r)
   ]

emptyLogin :: User -> Password -> LoginRequest
emptyLogin u p 
 = LoginRequest
    { lgName     = u
    , lgPassword = p
    , lgDomain   = Nothing
    }

data LoginResponse
    -- error responses, all together:
 = LoginError
    { lgSuccess  :: Bool
    , lgeError   :: String -- NeedToWait/NoName/Illegal/WrongPluginPass/NotExists/WrongPass/EmptyPass/future ones.
        -- (resist the temptation(?) to encode errors via a separate type, keep it loose for now in a String.
    , lgeDetails :: Maybe String
    , lgeWait    :: String
    }
    -- Successfully logged in, bundle up the result in a separate record so as to ease
    -- the carrying around of a user session. 
    --
    -- Notice the use of a shared boolean field betw. the constructors; eases the processing of
    -- responses in code where conds/guards are more convenient:
    --
    --    x <- submitLoginRequest lgReq
    --    when (not $ lgSuccess x) (handleError x)
    --    let sess = lgSession x
    --    ...
    --
 | LoginResponse
    { lgSuccess :: Bool
    , lgSession :: UserSession
    } 

