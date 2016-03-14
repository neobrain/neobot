--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.EmbeddedIn
-- Description : Representing 'embeddedin' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'embeddedin' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.EmbeddedIn where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data EmbeddedInRequest
 = EmbeddedInRequest
    { eiTitle        :: Maybe String
    , eiContinueFrom :: Maybe String
    , eiNamespace    :: [NamespaceID]
    , eiFilterRedir  :: Maybe Redirect
    , eiRedirect     :: Bool
    , eiLimit        :: Maybe Int
    }

instance APIRequest EmbeddedInRequest where
  queryKind _ = QList "embeddedin"
  showReq r = 
   [ mbOpt "eititle" id (eiTitle r)
   , mbOpt "eicontinue" id (eiContinueFrom r)
   , opt1  "einamespace" (eiNamespace r)
   , mbOpt "eifilterredir" id (eiFilterRedir r)
   , optB  "eirdirect" (eiRedirect r)
   , mbOpt "eilimit" show (eiLimit r)
   ]

emptyEmbeddedInRequest :: EmbeddedInRequest
emptyEmbeddedInRequest = EmbeddedInRequest
    { eiTitle        = Nothing
    , eiContinueFrom = Nothing
    , eiNamespace    = []
    , eiFilterRedir  = Nothing
    , eiRedirect     = False
    , eiLimit        = Nothing
    }

data EmbeddedInResponse
 = EmbeddedInResponse
    { eiLinks    :: [PageTitle]
    , eiContinue :: Maybe String
    }
    
emptyEmbeddedInResponse :: EmbeddedInResponse
emptyEmbeddedInResponse
 = EmbeddedInResponse
    { eiLinks    = []
    , eiContinue = Nothing
    }
