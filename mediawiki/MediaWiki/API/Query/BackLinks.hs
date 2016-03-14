--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.BackLinks
-- Description : Representing 'backlinks' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'backlinks' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.BackLinks where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data BackLinksRequest
 = BackLinksRequest
    { blTitle        :: Maybe String
    , blContinueFrom :: Maybe String
    , blNamespace    :: [NamespaceID]
    , blFilterRedir  :: Maybe Redirect
    , blRedirect     :: Maybe Bool
    , blLimit        :: Maybe Int
    }

instance APIRequest BackLinksRequest where
  queryKind _ = QList "backlinks"
  showReq r = 
   [ mbOpt "bltitle" id (blTitle r)
   , mbOpt "blcontinue" id (blContinueFrom r)
   , opt1  "blnamespace" (blNamespace r)
   , mbOpt "blfilterredir" id (blFilterRedir r)
   , optB  "blredirect" (fromMaybe False $ blRedirect r)
   , mbOpt "bllimit" show (blLimit r)
   ]

emptyBackLinksRequest :: BackLinksRequest
emptyBackLinksRequest = BackLinksRequest
    { blTitle        = Nothing
    , blContinueFrom = Nothing
    , blNamespace    = []
    , blFilterRedir  = Nothing
    , blRedirect     = Nothing
    , blLimit        = Nothing
    }

data BackLinksResponse
 = BackLinksResponse
    { blLinks    :: [PageTitle]
    , blContinue :: Maybe String
    }
    
emptyBackLinksResponse :: BackLinksResponse
emptyBackLinksResponse
 = BackLinksResponse
    { blLinks    = []
    , blContinue = Nothing
    }
