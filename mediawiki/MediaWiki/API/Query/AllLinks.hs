--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllLinks
-- Description : Representing 'alllinks' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'alllinks' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllLinks where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data AllLinksRequest
 = AllLinksRequest
    { alContinueFrom :: Maybe String
    , alFrom      :: Maybe PageName
    , alPrefix    :: Maybe PageName
    , alUnique    :: Bool
    , alProp      :: [String]
    , alNamespace :: Maybe NamespaceID
    , alLimit     :: Maybe Int
    }

instance APIRequest AllLinksRequest where
  queryKind _ = QList "alllinks"
  showReq r  =
   [ mbOpt "alcontinue" id (alContinueFrom r)
   , mbOpt "alfrom" id (alFrom r)
   , mbOpt "alprefix" id (alPrefix r)
   , optB  "alunique" (alUnique r)
   , opt1 "alprop" (alProp r)
   , mbOpt "alnamespace" id (alNamespace r)
   , mbOpt "allimit" show (alLimit r)
   ]

emptyAllLinksRequest :: AllLinksRequest
emptyAllLinksRequest = AllLinksRequest
    { alContinueFrom = Nothing
    , alFrom      = Nothing
    , alPrefix    = Nothing
    , alUnique    = False
    , alProp      = []
    , alNamespace = Nothing
    , alLimit     = Nothing
    }
  
data AllLinksResponse
 = AllLinksResponse
    { alLinks    :: [PageTitle]
    , alContinue :: Maybe String
    }
    
emptyAllLinksResponse :: AllLinksResponse
emptyAllLinksResponse
 = AllLinksResponse
    { alLinks    = []
    , alContinue = Nothing
    }
    
