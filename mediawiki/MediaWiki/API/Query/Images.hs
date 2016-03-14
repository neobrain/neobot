--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Images
-- Description : Representing 'images' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'images' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Images where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ImagesRequest
 = ImagesRequest
     { imLimit        :: Maybe Int
     , imContinueFrom :: Maybe String
     }

instance APIRequest ImagesRequest where
  queryKind _ = QProp "images"
  showReq r = 
   [ mbOpt "imlimit" show (imLimit r)
   , mbOpt "imcontinue" id (imContinueFrom r)
   ]

emptyImagesRequest :: ImagesRequest
emptyImagesRequest = ImagesRequest
  { imLimit        = Nothing
  , imContinueFrom = Nothing
  }
  
data ImagesResponse
 = ImagesResponse
     { imLinks    :: [(PageTitle,[PageTitle])]
     , imContinue :: Maybe String
     }
     
emptyImagesResponse :: ImagesResponse
emptyImagesResponse = ImagesResponse
  { imLinks    = []
  , imContinue = Nothing
  }
