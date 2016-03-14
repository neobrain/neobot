--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.AllImages
-- Description : Representing 'allimages' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'allimages' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.AllImages where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.ImageInfo ( ImageInfo )

data AllImagesRequest
 = AllImagesRequest
    { aiFrom            :: Maybe PageName
    , aiPrefix          :: Maybe PageName
    , aiMinSize         :: Maybe Int
    , aiMaxSize         :: Maybe Int
    , aiLimit           :: Maybe Int
    , aiDir             :: Maybe Direction
    , aiSha1            :: Maybe String
    , aiSha1Base36      :: Maybe String
    , aiProp            :: [String]
    }

instance APIRequest AllImagesRequest where
  queryKind _ = QList "allimages"
  showReq r
   = [ mbOpt "aifrom" id (aiFrom r)
     , mbOpt "aiprefix" id (aiPrefix r)
     , mbOpt "aiminsize" show (aiMinSize r)
     , mbOpt "aimaxsize" show (aiMaxSize r)
     , mbOpt "ailimit" show (aiLimit r)
     , mbOpt "aidir"   (\ x -> if x == Up then "ascending" else "descending")
                       (aiDir r)
     , mbOpt "aisha1"  id (aiSha1 r)
     , mbOpt "aisha1base36" id (aiSha1Base36 r)
     , opt1  "aiprop" (aiProp r)
     ]


emptyAllImagesRequest :: AllImagesRequest
emptyAllImagesRequest = AllImagesRequest
    { aiFrom            = Nothing
    , aiPrefix          = Nothing
    , aiMinSize         = Nothing
    , aiMaxSize         = Nothing
    , aiLimit           = Nothing
    , aiDir             = Nothing
    , aiSha1            = Nothing
    , aiSha1Base36      = Nothing
    , aiProp            = []
    }

  
data AllImagesResponse
 = AllImagesResponse
    { aiImages   :: [ImageInfo]
    , aiContinue :: Maybe String
    }
    
emptyAllImagesResponse :: AllImagesResponse
emptyAllImagesResponse
 = AllImagesResponse
    { aiImages   = []
    , aiContinue = Nothing
    }
