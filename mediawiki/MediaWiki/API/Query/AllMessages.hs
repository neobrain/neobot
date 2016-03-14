--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllMessages
-- Description : Representing 'allmessages' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'allmessages' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllMessages where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data AllMessagesRequest
 = AllMessagesRequest
    { amMessages        :: [String]
    , amFilter          :: Maybe String
    , amLang            :: Maybe String
    }

instance APIRequest AllMessagesRequest where
  queryKind _ = QMeta "allmessages"
  showReq r = [ opt1  "ammessages" (amMessages r)
              , mbOpt "amfilter" id (amFilter r)
	      , mbOpt "amlang" id (amLang r)
	      ]

emptyAllMessagesRequest :: AllMessagesRequest
emptyAllMessagesRequest = AllMessagesRequest
    { amMessages = []
    , amFilter   = Nothing
    , amLang     = Nothing
    }

  
data AllMessagesResponse
 = AllMessagesResponse
    { amsgMessages :: [MessageInfo]
    }
    
emptyAllMessagesResponse :: AllMessagesResponse
emptyAllMessagesResponse
 = AllMessagesResponse
    { amsgMessages = []
    }

data MessageInfo
 = MessageInfo
     { msgiName :: String
     , msgiMissing :: Bool
     , msgiContent :: String
     }
     
emptyMessageInfo :: MessageInfo
emptyMessageInfo = MessageInfo
     { msgiName = ""
     , msgiMissing = False
     , msgiContent = ""
     }
     
