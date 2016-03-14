--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Categories
-- Description : Representing 'categories' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'categories' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Categories 
       ( CategoriesRequest(..)
       , emptyCategoriesRequest
       
       , CategoriesResponse(..)
       , emptyCategoriesResponse
       ) where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data CategoriesRequest
 = CategoriesRequest
    { clProps        :: [PropKind]
    , clShow         :: Maybe Bool
    , clLimit        :: Maybe Int
    , clContinueFrom :: Maybe String
    }

instance APIRequest CategoriesRequest where
  queryKind _ = QProp "categories"
  showReq r = 
   [ opt1  "clprop" (map prKind $ clProps r)
   , mbOpt "clshow" (\ x -> (if x then ('!':) else id) "hidden") (clShow r)
   , mbOpt "cllimit" show (clLimit r)
   , mbOpt "clcontinue" id (clContinueFrom r)
   ]

emptyCategoriesRequest :: CategoriesRequest
emptyCategoriesRequest = CategoriesRequest
    { clProps = []
    , clShow  = Nothing
    , clLimit = Nothing
    , clContinueFrom = Nothing
    }

data CategoriesResponse
 = CategoriesResponse
    { clPages    :: [(PageTitle,[PageTitle])]
    , clContinue :: Maybe String
    }
    
emptyCategoriesResponse :: CategoriesResponse
emptyCategoriesResponse
 = CategoriesResponse
    { clPages    = []
    , clContinue = Nothing
    }
