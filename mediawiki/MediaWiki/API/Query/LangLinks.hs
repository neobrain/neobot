--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.LangLinks
-- Description : Representing 'langlinks' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'langlinks' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.LangLinks where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data LangLinksRequest
 = LangLinksRequest
     { llLimit        :: Maybe Int
     , llContinueFrom :: Maybe String
     }

instance APIRequest LangLinksRequest where
  queryKind _ = QProp "langlinks"
  showReq r
    = [ mbOpt "lllimit" show (llLimit r)
      , mbOpt "llcontinue" id (llContinueFrom r)
      ]

emptyLangLinksRequest :: LangLinksRequest
emptyLangLinksRequest = LangLinksRequest
  { llLimit        = Nothing
  , llContinueFrom = Nothing
  }
  
data LangLinksResponse
 = LangLinksResponse
     { llPages    :: [(PageTitle,[LangPageInfo])]
     , llContinue :: Maybe String
     }
     
emptyLangLinksResponse :: LangLinksResponse
emptyLangLinksResponse = LangLinksResponse
  { llPages    = []
  , llContinue = Nothing
  }

data LangPageInfo
 = LangPageInfo
     { langName  :: LangName
     , langTitle :: Maybe String
     }
     
emptyLangPageInfo :: LangPageInfo
emptyLangPageInfo = LangPageInfo
 { langName  = "en"
 , langTitle = Nothing
 }
  
