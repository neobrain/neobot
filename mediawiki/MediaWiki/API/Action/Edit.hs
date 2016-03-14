--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Edit
-- Description : Representing Edit requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Edit requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Edit where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data EditRequest
 = EditRequest
    { edTitle   :: Maybe PageName
    , edSection :: Maybe String
    , edText    :: Maybe String -- ^ Page content
    , edToken   :: Maybe Token  -- ^ Edit token. You can get one of these through prop=info
    , edSummary :: Maybe String
    , edIsMinor   :: Bool
    , edIsNonMinor :: Bool
    , edAsBot   :: Bool
    , edBaseTimestamp :: Maybe Timestamp
    , edRecreate :: Bool
    , edCreateOnly :: Bool
    , edNoCreate :: Bool
    , edCaptchaWord :: Maybe String
    , edCaptchaId   :: Maybe String
    , edWatch :: Bool
    , edUnwatch :: Bool
    , edMD5 :: Maybe String
    , edPrependText :: Maybe String
    , edAppendText :: Maybe String
    }

instance APIRequest EditRequest where
  isPostable _ = True
  showReq r = 
    [ mbOpt "title" id (edTitle r)
    , mbOpt "section" id (edSection r)
    , mbOpt "text" id (edText r)
    , mbOpt "token" id (edToken r)
    , mbOpt "summary" id (edSummary r)
    , optB "minor" (edIsMinor r)
    , optB "notminor" (edIsNonMinor r)
    , optB "bot" (edAsBot r)
    , mbOpt "basetimestamp" id (edBaseTimestamp r)
    , optB  "recreate" (edRecreate r)
    , optB  "createonly" (edCreateOnly r)
    , optB  "nocreate" (edNoCreate r)
    , mbOpt "captchaword" id (edCaptchaWord r)
    , mbOpt "captchaid" id (edCaptchaId r)
    , optB  "watch" (edWatch r)
    , optB  "unwatch" (edUnwatch r)
    , mbOpt "md5" id (edMD5 r)
    , mbOpt "prependtext" id (edPrependText r)
    , mbOpt "appendtext" id (edAppendText r)
    ]


emptyEditRequest :: EditRequest
emptyEditRequest = EditRequest
    { edTitle   = Nothing
    , edSection = Nothing
    , edText    = Nothing
    , edToken   = Nothing
    , edSummary = Nothing
    , edIsMinor   = False
    , edIsNonMinor = True
    , edAsBot   = False
    , edBaseTimestamp = Nothing
    , edRecreate = False
    , edCreateOnly = False
    , edNoCreate = False
    , edCaptchaWord = Nothing
    , edCaptchaId   = Nothing
    , edWatch = False
    , edUnwatch = False
    , edMD5 = Nothing
    , edPrependText = Nothing
    , edAppendText = Nothing
    }

