--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Types
-- Description : Basic MediaWiki API types
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Basic MediaWiki API types
-- 
--------------------------------------------------------------------
module MediaWiki.API.Types where

-- base types
type UserName = String
type NamespaceID = String
type TimeString = String
data Direction = Up | Down deriving ( Eq )
type RevID = String
type PageName = String
type UserID = String
data TimeArrow = Earlier | Later deriving ( Eq )
type Timestamp = String
type Redirect = String
type SortKind = String
type CatName = String
type GroupName = String
type FilterLang = String
type WithRedirects = String
type URLString = String
type Token = String
type LangName = String

nullRevId :: RevID
nullRevId = "0"

nullTimestamp :: Timestamp
nullTimestamp = ""

nullUser :: UserName
nullUser = ""

data PageTitle 
 = PageTitle { pgNS    :: NamespaceID
             , pgTitle :: Title 
	     , pgMbId  :: Maybe PageID
	     , pgMissing :: Bool
	     }

emptyPageTitle :: PageTitle
emptyPageTitle
 = PageTitle { pgNS    = mainNamespace
             , pgTitle = ""
	     , pgMbId  = Nothing
	     , pgMissing = False
	     }

mainNamespace :: NamespaceID
mainNamespace  = "0"

ns_MAIN :: NamespaceID
ns_MAIN = mainNamespace

ns_IMAGE :: NamespaceID
ns_IMAGE = "6"

data Format
 = Format 
     { formatKind      :: FormatKind
     , formatFormatted :: Bool
     }

emptyFormat :: Format
emptyFormat = Format{formatKind=FormatXML, formatFormatted=True}

xmlFormat :: Format
xmlFormat = emptyFormat{formatKind=FormatXML, formatFormatted=False}

data FormatKind
 = FormatJSON
 | FormatPHP 
 | FormatWDDX
 | FormatXML
 | FormatYAML
 | FormatTxt
 | FormatDbg

type User     = String
type Password = String
type LoginToken = String
type SessionID = String
type SessionToken = String
type ValueName = (String,String)

data UserInfo
 = UserInfo
    { uiName      :: UserName
    , uiId        :: UserID
    , uiIsAnon    :: Bool
    , uiHasMessage :: Bool
    , uiBlocked   :: Maybe (UserName,String)
    , uiGroups    :: [String]
    , uiRights    :: [String]
    , uiOptions   :: [(String,String)]
    , uiRateLimits :: [RateLimit]
    , uiEditCount  :: Maybe Int
    }

emptyUserInfo :: UserInfo
emptyUserInfo = UserInfo
    { uiName      = nullUser
    , uiId        = "0"
    , uiIsAnon    = True
    , uiHasMessage = False
    , uiBlocked   = Nothing
    , uiGroups    = []
    , uiRights    = []
    , uiOptions   = []
    , uiRateLimits = []
    , uiEditCount  = Nothing
    }

data RateLimit
 = RateLimit
    { rlName    :: String
    , rlHits    :: Int
    , rlSeconds :: Int
    }


data NamespaceInfo
 = NamespaceInfo
      { nsId       :: String
      , nsTitle    :: String
      , nsSubpages :: Bool
      }

data InterwikiEntry
 = InterwikiEntry
      { iwPrefix        :: String
      , iwLocal         :: Bool
      , iwTranscludable :: Maybe Bool
      , iwUrl           :: String
      , iwLanguage      :: Maybe String
      }

data UserSession
 = UserSession
    { sessUserId    :: UserID
    , sessUserName  :: UserName
    , sessPassword  :: Maybe Password  -- not sure; could leave out.
    , sessCookiePrefix :: Maybe String
    , sessSessionId :: Maybe SessionID
    , sessToken     :: LoginToken
    }

data HelpRequest
 = HelpRequest
    { helpVersion :: Maybe Bool
    }

type Title  = String   -- Q: what kind of encoding/escaping can be assumed here?
type PageID = String   -- numeric ID, so arguably wrong Haskell type.
type RevisionID = String -- ditto.

newtype PropKind 
 = PropKind { prKind :: String }
    {- Not deemed worthy to try to enumerate them all.
    -- Three major reasons: 
    --    - supportd properties are likely to evolve with MW API.
    --    - fields support subsets of the type, so using a union type
    --      for these is imprecise.
    --    - development of queries are driven by reading the API documentation
    --      from the MW API help page, so transliterations ought to be
    --      accommodated.
    --    - being too lazy to write them out is not a reason; did
    --      have such an enum type defined at one point :-)
    -}

newtype MetaKind
 = MetaKind { meKind :: String } -- likely values: siteinfo, userinfo, allmessages

newtype ListKind
 = ListKind { liKind :: String }

newtype GeneratorKind
 = GeneratorKind { genKind :: String }

class APIRequest a where
  showReq    :: a -> [Maybe (String,String)]
  isPostable :: a -> Bool
  isPostable _ = False
  
  queryKind :: a -> QueryKind
  queryKind _ = QProp ""


data QueryKind 
 = QProp String | QMeta String | QList String | QGen String
   deriving ( Eq )

data QueryRequest
 = QueryRequest
     { quTitles          :: [Title]
     , quPageIds         :: [PageID]
     , quRevIds          :: [RevisionID]
     , quProps           :: [PropKind]
     , quLists           :: [ListKind]
     , quMetas           :: [MetaKind]
     , quGenerator       :: Maybe GeneratorKind
     , quFollowRedirects :: Maybe Bool
     , quIndexPageIds    :: Maybe Bool
     }

emptyQuery :: QueryRequest
emptyQuery = QueryRequest
     { quTitles          = []
     , quPageIds         = []
     , quRevIds          = []
     , quProps           = []
     , quLists           = []
     , quMetas           = []
     , quGenerator       = Nothing
     , quFollowRedirects = Nothing
     , quIndexPageIds    = Nothing
     }
{-
data MetaKindProp
 = SiteInfoProp
 | UserInfoProp
 | AllMessagesProp
 | ExpandTemplatesProp
 | ParseProp
 | OpenSearchProp
 | FeedWatchlistProp
 | HelpProp
 | ParamInfoProp
 -}
