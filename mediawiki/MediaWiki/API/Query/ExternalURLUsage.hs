--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.ExternalURLUsage
-- Description : Representing 'exturlusage' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'exturlusage' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.ExternalURLUsage where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ExternalURLUsageRequest
 = ExternalURLUsageRequest
    { euProp       :: [String]
    , euOffset     :: Maybe String
    , euProtocol   :: Maybe String
    , euQuery      :: Maybe String
    , euNamespaces :: [NamespaceID]
    , euLimit      :: Maybe Int
    }

instance APIRequest ExternalURLUsageRequest where
  queryKind _ = QList "exturlusage"
  showReq r = 
    [ opt1 "euprop" (euProp r)
    , mbOpt "euoffset" id (euOffset r)
    , mbOpt "euprotocol" id (euProtocol r)
    , mbOpt "euquery" id (euQuery r)
    , opt1  "eunamespace" (euNamespaces r)
    , mbOpt "eulimit" show (euLimit r)
    ]

emptyExternalURLUsageRequest :: ExternalURLUsageRequest
emptyExternalURLUsageRequest = ExternalURLUsageRequest
    { euProp       = []
    , euOffset     = Nothing
    , euProtocol   = Nothing
    , euQuery      = Nothing
    , euNamespaces = []
    , euLimit      = Nothing
    }
    
data ExternalURLUsageResponse
 = ExternalURLUsageResponse
    { euPages    :: [(URLString,PageTitle)]
    , euContinue :: Maybe String
    }

emptyExternalURLUsageResponse :: ExternalURLUsageResponse
emptyExternalURLUsageResponse = ExternalURLUsageResponse
    { euPages    = []
    , euContinue = Nothing
    }
    

