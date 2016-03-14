--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Sitematrix
-- Description : Representing Sitematrix requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Sitematrix requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Sitematrix where

import MediaWiki.API.Types

-- 1.12+ and later
data SitematrixRequest
 = SitematrixRequest

emptySitematrixRequest :: SitematrixRequest
emptySitematrixRequest = SitematrixRequest

data SitematrixResponse
 = SitematrixResponse
     { smCount     :: Int
     , smSpecials  :: [SiteSpecialInfo]
     , smLanguages :: [LanguageInfo]
     }

data SiteSpecialInfo
 = SiteSpecialInfo
     { siCode :: String
     , siUrl  :: URLString
     }

data LanguageInfo
 = LanguageInfo
     { liCode  :: String
     , liName  :: String
     , liSites :: [SiteInfos]
     }

data SiteInfos
 = SiteInfos
     { siInfo :: String }
