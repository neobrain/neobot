--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllPages
-- Description : Representing 'allpages' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'allpages' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllPages where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data AllPagesRequest
 = AllPagesRequest
    { apFrom            :: Maybe PageName
    , apPrefix          :: Maybe PageName
    , apNamespace       :: NamespaceID
    , apFilterRedir     :: Maybe WithRedirects
    , apMinSize         :: Maybe Int
    , apMaxSize         :: Maybe Int
    , apProtTypeLevel   :: ([String],[String])
    , apLimit           :: Maybe Int
    , apDir             :: Maybe Direction
    , apFilterLangLinks :: Maybe FilterLang
    }

instance APIRequest AllPagesRequest where
  queryKind _ = QList "allpages"
  showReq r = 
    [ mbOpt "apfrom" id (apFrom r)
    , mbOpt "apprefix" id (apPrefix r)
    , mbOpt  "apnamespace" id (Just $ apNamespace r)
    , mbOpt "apfilterredir" id (apFilterRedir r)
    , mbOpt "apminsize" show (apMinSize r)
    , mbOpt "apmaxsize" show (apMaxSize r)
    , mbOpt "apprtype"  (piped.fst) (Just $ apProtTypeLevel r)
    , mbOpt "apprlevel" (piped.snd) (Just $ apProtTypeLevel r)
    , mbOpt "aplimit" show (apLimit r)
    , mbOpt "apdir"  (\ x -> if x==Up then "ascending" else "descending") (apDir r)
    , mbOpt "apfilterlanglinks" id (apFilterLangLinks r)
    ]

emptyAllPagesRequest :: AllPagesRequest
emptyAllPagesRequest = AllPagesRequest
    { apFrom            = Nothing
    , apPrefix          = Nothing
    , apNamespace       = mainNamespace
    , apFilterRedir     = Nothing
    , apMinSize         = Nothing
    , apMaxSize         = Nothing
    , apProtTypeLevel   = ([],[])
    , apLimit           = Nothing
    , apDir             = Nothing
    , apFilterLangLinks = Nothing
    }

  
data AllPagesResponse
 = AllPagesResponse
    { apLinks    :: [PageTitle] -- seems to have evolved now (1.13) to PageInfo.
    , apContinue :: Maybe String
    }
    
emptyAllPagesResponse :: AllPagesResponse
emptyAllPagesResponse
 = AllPagesResponse
    { apLinks    = []
    , apContinue = Nothing
    }
