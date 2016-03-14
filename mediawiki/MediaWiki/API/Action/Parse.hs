--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Action.Parse
-- Description : Representing Parse requests.
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Representing Parse requests.
-- 
--------------------------------------------------------------------
module MediaWiki.API.Action.Parse where

import MediaWiki.API.Types

data ParseRequest
 = ParseRequest
    { paTitle     :: Maybe PageName
    , paText      :: String
    , paPage      :: Maybe PageName
    , paOldId     :: Maybe RevID
    , paProp      :: [String]
    }

emptyParseRequest :: String -> ParseRequest
emptyParseRequest txt = ParseRequest
    { paTitle     = Nothing
    , paText      = txt
    , paPage      = Nothing
    , paOldId     = Nothing
    , paProp      = []
    }

data ParseResponse
 = ParseResponse
    { parText          :: String
    , parRevId         :: Maybe RevID
    , parLangLinks     :: Maybe [LanguageLink]
    , parCategories    :: Maybe [CategoryLink]
    , parLinks         :: Maybe [Link]
    , parTemplates     :: Maybe [Link]
    , parImages        :: Maybe [String]
    , parExternalLinks :: Maybe [URLString]
    , parSections      :: Maybe [TOCSection]
    }

emptyParseResponse :: ParseResponse
emptyParseResponse  = ParseResponse
    { parText          = ""
    , parRevId         = Nothing
    , parLangLinks     = Nothing
    , parCategories    = Nothing
    , parLinks         = Nothing
    , parTemplates     = Nothing
    , parImages        = Nothing
    , parExternalLinks = Nothing
    , parSections      = Nothing
    }

data LanguageLink
 = LanguageLink
    { laLang  :: String
    , laLink  :: String
    }

data CategoryLink
 = CategoryLink
    { caSortKey :: String
    , caLink    :: String
    }

data Link
 = Link
    { liNamespace :: String
    , liExists    :: Bool
    , liLink      :: String
    }

data TOCSection
 = TOCSection
    { tocTocLevel :: Int
    , tocLevel    :: Int
    , tocLine     :: String
    , tocNumber   :: String
    }

