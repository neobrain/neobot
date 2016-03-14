--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Revisions
-- Description : Representing 'revisions' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'revisions' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Revisions where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data RevisionRequest
 = RevisionRequest
    { rvProp            :: [String]
    , rvLimit           :: Maybe Int
    , rvStartID         :: Maybe RevID
    , rvEndID           :: Maybe RevID
    , rvStart           :: Maybe TimeString
    , rvEnd             :: Maybe TimeString
    , rvDir             :: Maybe Direction
    , rvUser            :: Maybe UserName
    , rvExcludeUser     :: Maybe UserName
    , rvExpandTemplates :: Maybe Bool
    , rvSection         :: Maybe String
    , rvTokens          :: [String]
    }

instance APIRequest RevisionRequest where
  queryKind _ = QProp "revisions"
  showReq r =
    [ opt1 "rvprop" (rvProp r)
    , mbOpt "rvlimit" show (rvLimit r)
    , mbOpt "rvstartid" id (rvStartID r)
    , mbOpt "rvendid" id (rvEndID r)
    , mbOpt "rvstart" id (rvStart r)
    , mbOpt "rvend"   id (rvEnd r)
    , mbOpt "rvdir" (\ x -> if x == Up then "ascending" else "descending")
                    (rvDir r)
    , mbOpt "rvuser" id (rvUser r)
    , mbOpt "rvexcludeuser" id (rvExcludeUser r)
    , optB  "rvexpandtemplates" (fromMaybe False (rvExpandTemplates r))
    , mbOpt "rvsection" id (rvSection r)
    , opt1  "rvtoken"  (rvTokens r)
    ]


emptyRevisionRequest :: RevisionRequest
emptyRevisionRequest = RevisionRequest
    { rvProp            = []
    , rvLimit           = Nothing
    , rvStartID         = Nothing
    , rvEndID           = Nothing
    , rvStart           = Nothing
    , rvEnd             = Nothing
    , rvDir             = Nothing
    , rvUser            = Nothing
    , rvExcludeUser     = Nothing
    , rvExpandTemplates = Nothing
    , rvTokens          = []
    , rvSection         = Nothing
    }

data RevisionsResponse
 = RevisionsResponse
    { rvPages    :: [(PageTitle,[Revision])]
    , rvContinue :: Maybe String
    }
    
emptyRevisionsResponse :: RevisionsResponse
emptyRevisionsResponse 
 = RevisionsResponse
    { rvPages    = []
    , rvContinue = Nothing
    }

data Revision
 = Revision
    { revPage          :: PageTitle
    , revRevId         :: RevID
    , revIsMinor       :: Bool
    , revUser          :: String
    , revIsAnon        :: Bool
    , revTimestamp     :: TimeString
    , revSize          :: Integer
    , revComment       :: Maybe String
--    , revRollbackToken :: Maybe Token
    , revContent       :: Maybe String
    }

emptyRevision :: PageTitle -> Revision
emptyRevision pg
 = Revision
    { revPage          = pg
    , revRevId         = nullRevId
    , revIsMinor       = False
    , revUser          = nullUser
    , revIsAnon        = False
    , revTimestamp     = nullTimestamp
    , revSize          = 0
    , revComment       = Nothing
    , revContent       = Nothing
    }
