--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Undelete
-- Description : Representing Undelete requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Undelete requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Undelete where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data UndeleteRequest
 = UndeleteRequest
    { udelTitle  :: PageName
    , udelToken  :: Token
    , udelReason :: Maybe String
    , udelTimestamps :: [Timestamp]
    }

instance APIRequest UndeleteRequest where
  isPostable _ = True
  showReq r = 
    [ opt "title" (udelTitle r)
    , opt "token" (udelToken r)
    , mbOpt "reason" id (udelReason r)
    , opt1 "timestamps" (udelTimestamps r)
    ]
emptyUndeleteRequest :: UndeleteRequest
emptyUndeleteRequest = UndeleteRequest
    { udelTitle = ""
    , udelToken = ""
    , udelReason = Nothing
    , udelTimestamps = []
    } 

