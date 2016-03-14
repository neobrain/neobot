--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.SiteInfo
-- Description : Representing 'siteinfo' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'siteinfo' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.SiteInfo where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data SiteInfoRequest
 = SiteInfoRequest
    { siProp       :: [String] -- general,namespaces,namespacealiases,
                               -- specialpagealiases,statistics,interwikimap,
			       -- dbrepllag,usergroups
    , siFilterIW   :: Maybe Bool
    , siShowAllDBs :: Bool
    }

instance APIRequest SiteInfoRequest where
  queryKind _ = QMeta "siteinfo"
  showReq r
   = [ opt1 "siprop" (siProp r)
     , mbOpt "sifilteriw" ( \x -> if x then "local" else "!local") (siFilterIW r)
     , optB "sishowalldb" (siShowAllDBs r)
     ]

emptySiteInfoRequest :: SiteInfoRequest
emptySiteInfoRequest = SiteInfoRequest
    { siProp        = []
    , siFilterIW    = Nothing
    , siShowAllDBs  = False
    }
    
data SiteInfoResponse 
 = SiteInfoResponse
    { siDBReplInfo         :: [DBInfo]
    , siNamespaces         :: [NamespaceInfo]
    , siGeneral            :: Maybe SiteInfo
    , siNamespaceAliases   :: [NamespaceInfo]
    , siSpecialPageAliases :: [(String,[String])]
    , siStatistics         :: Maybe SiteStatistics
    , siInterwiki          :: [InterwikiEntry]
    , siUserGroups         :: [UserGroup]
    }
    
emptySiteInfoResponse :: SiteInfoResponse
emptySiteInfoResponse = SiteInfoResponse
    { siDBReplInfo         = []
    , siNamespaces         = []
    , siGeneral            = Nothing
    , siNamespaceAliases   = []
    , siSpecialPageAliases = []
    , siStatistics         = Nothing
    , siInterwiki          = []
    , siUserGroups         = []
    }

type Permission = String

data UserGroup
 = UserGroup
    { ugName   :: String
    , ugRights :: [Permission]
    }

data SiteInfo
 = SiteInfo
      { siteMainPage   :: PageName
      , siteBase       :: URLString
      , siteName       :: String
      , siteGenerator  :: String
      , siteLastRevision :: Maybe String
      , siteCase       :: Maybe String
      , siteRightsCode :: Maybe String
      , siteRights     :: Maybe String
      , siteLang       :: Maybe String
      , siteFallbackEncoding :: Maybe String
      , siteWriteAPI   :: Bool
      , siteTimezone   :: Maybe String
      , siteTZOffset   :: Maybe Int
      }

data SiteStatistics
 = SiteStatistics
      { siPages    :: Integer
      , siArticles :: Integer
      , siViews    :: Integer
      , siEdits    :: Integer
      , siImages   :: Integer
      , siUsers    :: Integer
      , siAdmins   :: Integer
      , siJobs     :: Integer
      }



data DBInfo
 = DBInfo
      { dbHost :: String
      , dbLag  :: String
      }



