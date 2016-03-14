--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.LogEvents
-- Description : Representing 'logevents' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'logevents' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.LogEvents where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data LogEventsRequest
 = LogEventsRequest
    { leProp        :: [String]
    , leType        :: [String]
    , leStart       :: Maybe Timestamp
    , leEnd         :: Maybe Timestamp
    , leDir         :: Maybe TimeArrow
    , leUser        :: Maybe UserID
    , leTitle       :: Maybe PageName
    , leLimit      :: Maybe Int
    }

instance APIRequest LogEventsRequest where
  queryKind _ = QList "logevents"
  showReq r = 
    [ opt1 "leprop" (leProp r)
    , opt1 "letype" (leType r)
    , mbOpt "lestart" id (leStart r)
    , mbOpt "leend"   id (leEnd r)
    , mbOpt "ledir" (\ x -> if x==Earlier then "older" else "newer") (leDir r)
    , mbOpt "leuser" id (leUser r)
    , mbOpt "letitle" id (leTitle r)
    , mbOpt "lelimit" show (leLimit r)
    ]

emptyLogEventsRequest :: LogEventsRequest
emptyLogEventsRequest = LogEventsRequest
    { leProp        = []
    , leType        = []
    , leStart       = Nothing
    , leEnd         = Nothing
    , leDir         = Nothing
    , leUser        = Nothing
    , leTitle       = Nothing
    , leLimit       = Nothing
    }

data LogEventsResponse
 = LogEventsResponse
    { leEvents   :: [LogEvent]
    , leContinue :: Maybe String
    }

emptyLogEventsResponse :: LogEventsResponse
emptyLogEventsResponse = LogEventsResponse
    { leEvents   = []
    , leContinue = Nothing
    }
    
data LogEvent
 = LogEvent
    { levLogId     :: Maybe String
    , levPage      :: PageTitle
    , levType      :: Maybe String
    , levAction    :: Maybe String
    , levParams    :: [LogEventParam]
    , levUser      :: Maybe String
    , levTimestamp :: Maybe Timestamp
    , levComment   :: Maybe String
    }
    
emptyLogEvent :: LogEvent
emptyLogEvent 
 = LogEvent
    { levLogId     = Nothing
    , levPage      = emptyPageTitle
    , levType      = Nothing
    , levAction    = Nothing
    , levParams    = []
    , levUser      = Nothing
    , levTimestamp = Nothing
    , levComment   = Nothing
    }

data LogEventParam
 = LogEventMove
     { levMovePage  :: PageTitle }
 | LogEventPatrol 
     { levPatrolCurrent  :: Maybe String
     , levPatrolPrevious :: Maybe String
     , levPatrolAuto     :: Maybe String
     }
 | LogEventRights
     { levRightsOld      :: Maybe String
     , levRightsNew      :: Maybe String
     }
 | LogEventBlock
     { levBlockDuration  :: Maybe String
     , levBlockFlags     :: Maybe String
     }
 | LogEventParam {levParamValue :: String }
 | LogEventOther 
    { levParamName    :: String
    , levParamAttrs   :: [(String,String)]
    }
