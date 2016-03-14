--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Delete
-- Description : Representing Delete requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Delete requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Delete where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data DeleteRequest
 = DeleteRequest
    { delTitle  :: PageName
    , delToken  :: Token
    , delReason :: Maybe String
    , delWatch  :: Maybe Bool
    , delUnwatch :: Maybe Bool 
    , delOldImage :: Maybe String
    }

instance APIRequest DeleteRequest where
  isPostable _ = True
  showReq r = 
    [ opt "title" (delTitle r)
    , opt "token" (delToken r)
    , mbOpt "reason" id (delReason r)
    , optB "watch" (fromMaybe False $ delWatch r)
    , optB "unwatch" (fromMaybe False $ delUnwatch r)
    , mbOpt "oldimage" id (delOldImage r)
    ]

emptyDeleteRequest :: DeleteRequest
emptyDeleteRequest = DeleteRequest
    { delTitle = ""
    , delToken = ""
    , delReason = Nothing
    , delWatch = Nothing
    , delUnwatch = Nothing
    , delOldImage = Nothing
    } 

