--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.CategoryMembers
-- Description : Representing 'categorymembers' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'categorymembers' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.CategoryMembers where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data CategoryMembersRequest
 = CategoryMembersRequest
    { cmTitle        :: Maybe PageName
    , cmProp         :: [String]
    , cmNamespace    :: [NamespaceID]
    , cmContinueFrom :: Maybe String
    , cmLimit        :: Maybe Int
    , cmSort         :: Maybe SortKind
    , cmDir          :: Maybe Direction
    , cmStart        :: Maybe Timestamp
    , cmEnd          :: Maybe Timestamp
    }

instance APIRequest CategoryMembersRequest where
  queryKind _ = QList "categorymembers"
  showReq r = 
    [ mbOpt "cmtitle" id (cmTitle r)
    , opt1  "cmprop" (cmProp r)
    , opt1  "cmnamespace" (cmNamespace r)
    , mbOpt "cmcontinue" id (cmContinueFrom r)
    , mbOpt "cmlimit" show (cmLimit r)
    , mbOpt "cmsort" id (cmSort r)
    , mbOpt "cmdir"  (\ x -> if x==Up then "asc" else "desc") (cmDir r)
    , mbOpt "cmstart" id (cmStart r)
    , mbOpt "cmend" id (cmEnd r)
    ]

emptyCategoryMembersRequest :: CategoryMembersRequest
emptyCategoryMembersRequest = CategoryMembersRequest
    { cmTitle        = Nothing
    , cmProp         = []
    , cmNamespace    = []
    , cmContinueFrom = Nothing
    , cmLimit        = Nothing
    , cmSort         = Nothing
    , cmDir          = Nothing
    , cmStart        = Nothing
    , cmEnd          = Nothing
    }

data CategoryMembersResponse
 = CategoryMembersResponse
    { cmPages    :: [PageTitle]
    , cmContinue :: Maybe String
    }
    
emptyCategoryMembersResponse :: CategoryMembersResponse
emptyCategoryMembersResponse = CategoryMembersResponse
    { cmPages = []
    , cmContinue = Nothing
    }
    
