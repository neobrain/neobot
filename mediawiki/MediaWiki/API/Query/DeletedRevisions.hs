--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.DeletedRevisions
-- Description : Representing 'deletedrevs' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'deletedrevs' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.DeletedRevisions where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data DeletedRevisionsRequest
 = DeletedRevisionsRequest
    { drStart       :: Maybe Timestamp
    , drEnd         :: Maybe Timestamp
    , drDir         :: Maybe TimeArrow
    , drLimit       :: Maybe Int
    , drProp        :: [String]
    }

instance APIRequest DeletedRevisionsRequest where
  queryKind _ = QList "deletedrevs"
  showReq r = [ mbOpt   "drstart" id (drStart r)
              , mbOpt   "drend"   id (drEnd r)
	      , mbOpt   "drdir"   (\ x -> if x==Earlier then "older" else "newer")
	                (drDir r)
	      , mbOpt   "drlimit" show (drLimit r)
	      , opt1    "drprop"  (drProp r)
	      ]

emptyDeletedRevisionsRequest :: DeletedRevisionsRequest
emptyDeletedRevisionsRequest = DeletedRevisionsRequest
    { drStart       = Nothing
    , drEnd         = Nothing
    , drDir         = Nothing
    , drLimit       = Nothing
    , drProp        = []
    }

data DeletedRevisionsResponse
 = DeletedRevisionsResponse
    { drRevisions :: [DeletedRevision]
    , drContinue  :: Maybe String
    }

emptyDeletedRevisionsResponse :: DeletedRevisionsResponse
emptyDeletedRevisionsResponse = DeletedRevisionsResponse
    { drRevisions = []
    , drContinue  = Nothing
    }

data DeletedRevision
 = DeletedRevision
    { drPage      :: PageTitle
    , drTimestamp :: Maybe Timestamp
    , drRevId     :: Maybe RevID
    , drUser      :: Maybe UserName
    , drComment   :: Maybe String
    , drIsMinor   :: Bool
    , drLength    :: Maybe Int
    , drContent   :: Maybe String
    , drToken     :: Maybe String
    }
    
emptyDeletedRevision :: DeletedRevision
emptyDeletedRevision 
 = DeletedRevision
    { drPage      = emptyPageTitle
    , drTimestamp = Nothing
    , drRevId     = Nothing
    , drUser      = Nothing
    , drComment   = Nothing
    , drIsMinor   = False
    , drLength    = Nothing
    , drContent   = Nothing
    , drToken     = Nothing
    }
