--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Move
-- Description : Representing Move requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Move requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Move where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data MoveRequest
 = MoveRequest
    { mvFrom    :: Maybe PageName
    , mvTo      :: Maybe PageName
    , mvToken   :: Token
    , mvReason  :: Maybe String
    , mvMoveTalk :: Bool
    , mvNoRedir  :: Bool
    , mvWatch    :: Bool
    , mvUnwatch  :: Bool
    }

instance APIRequest MoveRequest where
 isPostable _ = True
 showReq r = 
    [ mbOpt "from" id (mvFrom r)
    , mbOpt "to"   id (mvTo r)
    , opt   "token" (mvToken r)
    , mbOpt "reason" id (mvReason r)
    , optB "movetalk" (mvMoveTalk r)
    , optB "noredirect"  (mvNoRedir r)
    , optB "watch" (mvWatch r)
    , optB "unwatch" (mvUnwatch r)
    ]


emptyMoveRequest :: MoveRequest
emptyMoveRequest = MoveRequest
    { mvFrom    = Nothing
    , mvTo      = Nothing
    , mvToken   = ""
    , mvReason  = Nothing
    , mvMoveTalk = False
    , mvNoRedir  = False
    , mvWatch    = False
    , mvUnwatch  = False
    }

