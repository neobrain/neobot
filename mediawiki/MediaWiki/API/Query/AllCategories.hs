--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllCategories
-- Description : Representing 'allcategories' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'allcategories' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllCategories where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.CategoryInfo ( CategoryInfo )

data AllCategoriesRequest
 = AllCategoriesRequest
    { acFrom            :: Maybe PageName
    , acPrefix          :: Maybe PageName
    , acDir             :: Maybe Direction
    , acLimit           :: Maybe Int
    , acProp            :: [String]
    }

instance APIRequest AllCategoriesRequest where
  queryKind _ = QList "allcategories"
  showReq r 
   = [ mbOpt "acfrom" id (acFrom r)
     , mbOpt "acprefix" id (acPrefix r)
     , mbOpt "acdir" (\ x -> if x == Up then "ascending" else "descending")
                     (acDir r)
     , mbOpt "aclimit" show (acLimit r)
     , opt1   "acprop" (acProp r)
     ]

emptyAllCategoriesRequest :: AllCategoriesRequest
emptyAllCategoriesRequest = AllCategoriesRequest
    { acFrom            = Nothing
    , acPrefix          = Nothing
    , acDir             = Nothing
    , acLimit           = Nothing
    , acProp            = []
    }

  
data AllCategoriesResponse
 = AllCategoriesResponse
    { acCategories :: [CategoryInfo]
    , acContinue   :: Maybe String
    }
    
emptyAllCategoriesResponse :: AllCategoriesResponse
emptyAllCategoriesResponse
 = AllCategoriesResponse
    { acCategories = []
    , acContinue   = Nothing
    }
