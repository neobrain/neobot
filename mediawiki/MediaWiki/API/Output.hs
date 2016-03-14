--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API.Output
-- Description : Serializing MediaWiki API types
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- Serializing MediaWiki API types
-- 
--------------------------------------------------------------------
module MediaWiki.API.Output where

import MediaWiki.API.Types
import MediaWiki.API.Base

import MediaWiki.Util.Codec.Percent

import Data.List
import Data.Maybe

showRequest :: Request -> String
showRequest req = 
  join (showAction (reqAction req) : showMaxLag (reqMaxLag req) (showFormat (reqFormat req)))

showMaxLag :: Maybe Int -> String -> [String]
showMaxLag Nothing  rs = [rs]
showMaxLag (Just l) rs = [field "maxlag" (show l), rs]

showFormat :: Format -> String
showFormat f = field "format" $ isFormatted (formatFormatted f) $
 case formatKind f of
   FormatJSON -> "json"
   FormatPHP  -> "php"
   FormatWDDX -> "wddx"
   FormatXML  -> "xml"
   FormatYAML -> "yaml"
   FormatTxt  -> "txt"
   FormatDbg  -> "dbg"
 where
  isFormatted False xs = xs
  isFormatted _     xs = xs++"fm"
  
toReq :: APIRequest a => a -> [String]
toReq x =
 case catMaybes $ showReq x of
   [] -> []
   xs -> map showP xs
 where
  showP (a,b) = a ++ '=':b

showQueryRequestKind :: QueryRequestKind -> [String]
showQueryRequestKind r = 
 case r of
   InfoProp p -> toReq p
   RevisionsProp p -> toReq p
   LinksPropProp p -> toReq p
   LangLinksProp p -> toReq p
   ImagesProp    p -> toReq p
   ImageInfoProp p -> toReq p
   TemplatesProp p -> toReq p
   CategoriesProp p -> toReq p
   AllCategoriesProp p -> toReq p
   AllImagesProp     p -> toReq p
   AllLinksProp      p -> toReq p
   AllMessagesProp   p -> toReq p
   AllPagesProp      p -> toReq p
   AllUsersProp      p -> toReq p
   BacklinksProp     p -> toReq p
   EmbeddedInProp    p -> toReq p
   ImageUsageProp    p -> toReq p
   CategoryInfoProp  p -> toReq p
   CategoryMembersProp p -> toReq p
   ExternalLinksProp    p -> toReq p
   ExternalURLUsageProp p -> toReq p
   LogEventsProp        p -> toReq p
   RecentChangesProp    p -> toReq p
   SearchProp           p -> toReq p
   SiteInfoProp         p -> toReq p
   UserContribsProp     p -> toReq p
   UserInfoProp         p -> toReq p
   WatchListProp        p -> toReq p
   BlocksProp           p -> toReq p
   DeletedRevsProp      p -> toReq p
   UsersProp            p -> toReq p
   RandomProp           p -> toReq p
 

showAction :: Action -> String
showAction act = join $
  case act of
    Sitematrix -> [field "action" "sitematrix"]
    Login l    -> (field "action" "login"): (toReq l)
    Logout     -> [field "action" "logout"]
    Query q qr -> (field "action" "query") : (showQuery q) ++ qr
    ExpandTemplates et -> (field "action" "expandtemplates") : (showExpandTemplates et)
    Parse pt -> (field "action" "parse") : (showParseRequest pt)
    OpenSearch os ->  (field "action" "opensearch") : (showOpenSearch os)
    FeedWatch f -> (field "action" "feedwatchlist") : (showFeedWatch f)
    Help h -> (field "action" "help") : (showHelpRequest h)
    ParamInfo pin ->  (field "action" "paraminfo") : (showParamInfoRequest pin)
    Unblock r -> (field "action" "unblock") : toReq r
    Watch   r -> (field "action" "watch") : toReq r
    EmailUser r -> (field "action" "emailuser") : toReq r
    Edit      r -> (field "action" "edit") : toReq r
    Move      r -> (field "action" "move") : toReq r
    Block     r -> (field "action" "block") : toReq r
    Protect   r -> (field "action" "protect") : toReq r
    Undelete  r -> (field "action" "undelete") : toReq r
    Delete    r -> (field "action" "delete") : toReq r
    Rollback  r -> (field "action" "rollback") : toReq r
    OtherAction  at vs -> (field "action" at) : (map showValueName vs)

{-
showLoginRequest :: LoginRequest -> [String]
showLoginRequest l = catMaybes 
  [ Just $ field "name" (lgName l)
  , Just $ field "password" (lgPassword l)
  , fieldMb "version" id (lgDomain l)
  ]
-}

showHelpRequest :: HelpRequest -> [String]
showHelpRequest h = 
  maybeToList (fieldMb "version" showBool (helpVersion h))

showQuery :: QueryRequest -> [String]
showQuery q = catMaybes
  [ fList    "titles"  (quTitles q)
  , fList    "pageids" (quPageIds q)
  , fList    "revids"  (quRevIds q)
  , fList    "prop"    (map showPropKind $ quProps q)
  , fList    "list"    (map showListKind $ quLists q)
  , fList    "meta"    (map showMetaKind $ quMetas q)
  , fieldMb  "generator"    showGeneratorKind  (quGenerator q)
  , fieldMb  "redirects"    showBool           (quFollowRedirects q)
  , fieldMb  "indexpageids" showBool           (quIndexPageIds q)
  ]
 where
  fList t ls = fieldList t "|" ls

showExpandTemplates :: ExpandTemplatesRequest -> [String]
showExpandTemplates et = catMaybes
  [ fieldMb "title" id (etTitle et)
  , Just $ field "text" (etText et)
  ]
  
showPropKind :: PropKind -> String
showPropKind pk = prKind pk

showListKind :: ListKind -> String
showListKind lk = liKind lk

showMetaKind :: MetaKind -> String
showMetaKind mk = meKind mk

showParseRequest :: ParseRequest -> [String]
showParseRequest p = catMaybes
  [ fieldMb "title" id (paTitle p)
  , Just $ field "text" (paText p)
  , fieldMb "page" id (paPage p)
  , fieldList "prop" "|" (paProp p)
  ]
  
showOpenSearch :: OpenSearchRequest -> [String]
showOpenSearch o = catMaybes
  [ Just $ field "search" (osSearch o)
  , fieldMb "limit" show (osLimit o)
  ]
  
showFeedWatch :: FeedWatchListRequest -> [String]
showFeedWatch f = catMaybes
 [ Just $ field "feedformat" (if feAsAtom f then "atom" else "rss")
 , fieldMb "hours" show (feHours f)
 , if feAllRev f then (Just $ field  "allrev" "") else Nothing
 ]
 
showParamInfoRequest :: ParamInfoRequest -> [String]
showParamInfoRequest p = catMaybes
 [ fieldList "modules" "," (paModules p)
 , fieldList "querymodules" "|" (paQueryModules p)
 ]
 
showGeneratorKind :: GeneratorKind -> String
showGeneratorKind gk = genKind gk

showValueName :: ValueName -> String
showValueName (n,v) = field n v

showBool :: Bool -> String
showBool True = "true"
showBool _    = "false"

join :: [String] -> String
join [] = ""
join xs = concat (intersperse "&" xs)

field :: String -> String -> String
field a b = a ++ '=':b

fieldMb :: String -> (a -> String) -> Maybe a -> Maybe String
fieldMb _ _ Nothing = Nothing
fieldMb n f (Just x) = Just (field n (f x))

fieldList :: String -> String -> [String] -> Maybe String
fieldList _ _ [] = Nothing
fieldList n s xs = Just (field n (intercalate (getEncodedString s) xs))


