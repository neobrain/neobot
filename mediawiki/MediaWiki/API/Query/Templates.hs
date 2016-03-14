--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Templates
-- Description : Representing 'templates' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'templates' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Templates where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data TemplatesRequest
 = TemplatesRequest
    { tlNamespaces   :: [NamespaceID]
    , tlLimit        :: Maybe Int
    , tlContinueFrom :: Maybe String
    }

instance APIRequest TemplatesRequest where
  queryKind _ = QProp "templates"
  showReq r
   = [ opt1 "tlnamespace" (tlNamespaces r)
     , mbOpt "tllimit" show (tlLimit r)
     , mbOpt "tlcontinue" id (tlContinueFrom r)
     ]

emptyTemplatesRequest :: TemplatesRequest
emptyTemplatesRequest = TemplatesRequest
    { tlNamespaces = []
    , tlLimit      = Nothing
    , tlContinueFrom = Nothing
    }
    
data TemplatesResponse 
 = TemplatesResponse
    { tlPages :: [(PageTitle,[PageTitle])]
    , tlContinue :: Maybe String
    }
    
emptyTemplatesResponse :: TemplatesResponse
emptyTemplatesResponse = TemplatesResponse
    { tlPages = []
    , tlContinue = Nothing
    }
    


