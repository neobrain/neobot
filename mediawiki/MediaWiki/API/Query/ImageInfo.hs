--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.ImageInfo
-- Description : Representing 'imageinfo' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'imageinfo' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.ImageInfo where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data ImageInfo
 = ImageInfo
    { iiTimestamp :: Timestamp
    , iiUser      :: UserName
    , iiWidth     :: Maybe Int
    , iiHeight    :: Maybe Int
    , iiSize      :: Maybe Int
    , iiURL       :: Maybe URLString
    , iiComment   :: Maybe String
    , iiSHA1      :: Maybe String
    , iiArchive   :: Maybe String
    , iiBitDepth  :: Maybe Int
    , iiMime      :: Maybe String
    }
    
emptyImageInfo :: ImageInfo
emptyImageInfo = ImageInfo
    { iiTimestamp = nullTimestamp
    , iiUser      = nullUser
    , iiWidth     = Nothing
    , iiHeight    = Nothing
    , iiSize      = Nothing
    , iiURL       = Nothing
    , iiComment   = Nothing
    , iiSHA1      = Nothing
    , iiArchive   = Nothing
    , iiBitDepth  = Nothing
    , iiMime      = Nothing
    }

data ImageInfoRequest
 = ImageInfoRequest 
    { iiProp      :: [PropKind]
    , iiLimit     :: Maybe Int
    , iiStart     :: Maybe Timestamp
    , iiEnd       :: Maybe Timestamp
    , iiURLSize   :: Maybe (Int,Int)
    }

instance APIRequest ImageInfoRequest where
 queryKind _ = QProp "imageinfo"
 showReq r =
  [ opt1 "iiprop" (map prKind $ iiProp r)
  , mbOpt "iilimit" show (iiLimit r)
  , mbOpt "iistart" id (iiStart r)
  , mbOpt "iiend" id (iiEnd r)
  , mbOpt "iiurlwidth" (show.fst) (iiURLSize r)
  , mbOpt "iiurlheight" (show.snd) (iiURLSize r)
  ]

emptyImageInfoRequest :: ImageInfoRequest
emptyImageInfoRequest = ImageInfoRequest
    { iiProp      = []
    , iiLimit     = Nothing
    , iiStart     = Nothing
    , iiEnd       = Nothing
    , iiURLSize   = Nothing
    }

data ImageInfoResponse 
 = ImageInfoResponse
    { iiPages    :: [(PageTitle,[ImageInfo])]
    , iiContinue :: Maybe String
    }

emptyImageInfoResponse :: ImageInfoResponse
emptyImageInfoResponse = ImageInfoResponse
    { iiPages = []
    , iiContinue = Nothing
    }
    
