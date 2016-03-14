--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Rollback
-- Description : Representing Rollback requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Rollback requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Rollback where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data RollbackRequest
 = RollbackRequest
    { rbTitle :: PageName
    , rbUser  :: UserName
    , rbToken :: Token
    , rbSummary :: Maybe String
    , rbMarkBot :: Maybe Bool
    }

instance APIRequest RollbackRequest where
  isPostable _ = True
  showReq r = 
    [ opt "title" (rbTitle r)
    , opt "user"  (rbUser r)
    , opt "token" (rbToken r)
    , mbOpt "summary" id (rbSummary r)
    , mbOpt "markbot" (\ x -> if x then "1" else "0") (rbMarkBot r)
    ] 

emptyRollbackRequest :: RollbackRequest
emptyRollbackRequest = RollbackRequest
    { rbTitle = ""
    , rbUser  = ""
    , rbToken = ""
    , rbSummary = Nothing
    , rbMarkBot = Nothing
    } 

