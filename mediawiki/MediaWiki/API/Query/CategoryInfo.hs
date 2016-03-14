--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.CategoryInfo
-- Description : Representing CategoryInfo requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing CategoryInfo requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.CategoryInfo where

import MediaWiki.API.Types
import MediaWiki.API.Utils ()

data CategoryInfoRequest
 = CategoryInfoRequest
 
instance APIRequest CategoryInfoRequest where
  queryKind _ = QProp "categoryinfo"
  showReq _ = []

emptyCategoryInfoRequest :: CategoryInfoRequest
emptyCategoryInfoRequest = CategoryInfoRequest

data CategoryInfoResponse 
 = CategoryInfoResponse
     { ciPages :: [CategoryInfo]
     }
     
emptyCategoryInfoResponse :: CategoryInfoResponse
emptyCategoryInfoResponse = CategoryInfoResponse
     { ciPages = []
     }

data CategoryInfo
 = CategoryInfo
     { ciPage       :: PageTitle
     , ciSize       :: Maybe Int
     , ciPageSize   :: Maybe Int
     , ciFiles      :: Maybe Int
     , ciSubCats    :: Maybe Int
     , ciHidden     :: Bool
     }

emptyCategoryInfo :: CategoryInfo
emptyCategoryInfo = CategoryInfo
     { ciPage = emptyPageTitle
     , ciSize = Nothing
     , ciPageSize = Nothing
     , ciFiles = Nothing
     , ciSubCats = Nothing
     , ciHidden = False
     }
     
