--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.ImageUsage
-- Description : Representing 'imageusage' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'imageusage' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.ImageUsage where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ImageUsageRequest
 = ImageUsageRequest
    { iuTitle        :: Maybe String
    , iuContinueFrom :: Maybe String
    , iuNamespace    :: [NamespaceID]
    , iuFilterRedir  :: Maybe Redirect
    , iuRedirect     :: Bool
    , iuLimit        :: Maybe Int
    }

instance APIRequest ImageUsageRequest where
  queryKind _ = QList "imageusage"
  showReq r = 
   [ mbOpt "iutitle" id (iuTitle r)
   , mbOpt "iucontinue" id (iuContinueFrom r)
   , opt1 "iunamespace" (iuNamespace r)
   , mbOpt "iufilterredir" id (iuFilterRedir r)
   , optB  "iuredirect" (iuRedirect r)
   , mbOpt "iulimit" show (iuLimit r)
   ]

emptyImageUsageRequest :: ImageUsageRequest
emptyImageUsageRequest = ImageUsageRequest
    { iuTitle        = Nothing
    , iuContinueFrom = Nothing
    , iuNamespace    = []
    , iuFilterRedir  = Nothing
    , iuRedirect     = False
    , iuLimit        = Nothing
    }

data ImageUsageResponse
 = ImageUsageResponse
    { iuLinks    :: [PageTitle]
    , iuContinue :: Maybe String
    }
    
emptyImageUsageResponse :: ImageUsageResponse
emptyImageUsageResponse
 = ImageUsageResponse
    { iuLinks    = []
    , iuContinue = Nothing
    }
