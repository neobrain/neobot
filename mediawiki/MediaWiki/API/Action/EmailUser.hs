--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.EmailUser
-- Description : Representing EmailUser requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing EmailUser requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.EmailUser where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data EmailUserRequest
 = EmailUserRequest
    { emTarget  :: Maybe String
    , emSubject :: Maybe String
    , emText    :: Maybe String
    , emToken   :: Maybe Token
    , emCcMe    :: Bool
    }

instance APIRequest EmailUserRequest where
  isPostable _ = True
  showReq r = 
   [ mbOpt "target" id (emTarget r)
   , mbOpt "subject" id (emSubject r)
   , mbOpt "text" id (emText r)
   , mbOpt "token" id (emToken r)
   , optB  "ccme" (emCcMe r)
   ]

emptyEmailUserRequest :: EmailUserRequest
emptyEmailUserRequest = EmailUserRequest
    { emTarget  = Nothing
    , emSubject = Nothing
    , emText    = Nothing
    , emToken   = Nothing
    , emCcMe    = False
    }
