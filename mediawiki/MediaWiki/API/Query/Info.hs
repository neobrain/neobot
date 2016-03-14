--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Query.Info
-- Description : Representing 'info' requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing 'info' requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Query.Info where

import MediaWiki.API.Types
import MediaWiki.API.Utils

data InfoRequest
 = InfoRequest
     { inProps  :: [String]
     , inTokens :: [String]
     }

instance APIRequest InfoRequest where
  queryKind _ = QProp "info"
  showReq r = 
   [ opt1 "inprop" (inProps r)
   , opt1 "intoken" (inTokens r)
   ]

emptyInfoRequest :: InfoRequest
emptyInfoRequest = InfoRequest
     { inProps  = []
     , inTokens = []
     }

data InfoResponse
 = InfoResponse
     { infPages :: [InfoPage]
     }
     
data InfoPage
 = InfoPage
     { infPage       :: PageTitle
     , infTouched    :: TimeString
     , infLastRevId  :: RevID
     , infCounter    :: Integer
     , infLength     :: Integer
     , infIsRedirect :: Bool
     , infIsNew      :: Bool
     , infEditTok    :: Maybe Token
     , infDeleteTok  :: Maybe Token
     , infProtectTok :: Maybe Token
     , infMoveTok    :: Maybe Token
     , infProtection :: [PageRestriction]
     }

emptyInfoResponse :: InfoResponse
emptyInfoResponse =
   InfoResponse{ infPages= []
               }

emptyInfoPage :: InfoPage
emptyInfoPage =
   InfoPage
     { infPage       = emptyPageTitle
     , infTouched    = ""
     , infLastRevId  = ""
     , infCounter    = 0
     , infLength     = 0
     , infIsRedirect = False
     , infIsNew      = False
     , infEditTok    = Nothing
     , infDeleteTok  = Nothing
     , infProtectTok = Nothing
     , infMoveTok    = Nothing
     , infProtection = []
     }

data PageRestriction
 = PageRestriction
     { prPageId  :: PageID
     , prSource  :: PageName
     , prType    :: String
     , prLevel   :: Integer
     , prExpiry  :: TimeString
     , prCascade :: Bool
     }
