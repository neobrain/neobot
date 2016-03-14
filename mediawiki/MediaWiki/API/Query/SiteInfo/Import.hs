module MediaWiki.API.Query.SiteInfo.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.SiteInfo

import Text.XML.Light.Types
import Text.XML.Light.Proc   ( strContent )

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) SiteInfoResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe SiteInfoResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "query" es1
  let es = children p
  let dbs  = fromMaybe [] $ fmap (mapMaybe xmlDB) (fmap children $ pNode "dblrepllag" es)
  let nss  = fromMaybe [] $ fmap (mapMaybe xmlNS) (fmap children $ pNode "namespaces" es)
  let nass = fromMaybe [] $ fmap (mapMaybe xmlNS) (fmap children $ pNode "namespacealiases" es)
  let gen  = pNode "general" es >>= xmlSI
  let ss   = fromMaybe [] $ fmap (mapMaybe xmlSS) (fmap children $ pNode "specialpagealiases" es)
  let st   =  pNode "statistics" es >>= xmlStat
  let iws  = fromMaybe [] $ fmap (mapMaybe xmlIW) (fmap children $ pNode "interwikimap" es)
  let ugs  = fromMaybe [] $ fmap (mapMaybe xmlGr) (fmap children $ pNode "usergroups" es)
  return 
   emptySiteInfoResponse
    { siDBReplInfo         = dbs
    , siNamespaces         = nss
    , siGeneral            = gen
    , siNamespaceAliases   = nass
    , siSpecialPageAliases = ss
    , siStatistics         = st
    , siInterwiki          = iws
    , siUserGroups         = ugs
    }

xmlDB :: Element -> Maybe DBInfo
xmlDB e = do
   guard (elName e == nsName "db")
   let h    = fromMaybe ""  $ pAttr "host" e
   let l    = fromMaybe ""  $ pAttr "lag" e 
   return DBInfo{dbHost=h,dbLag=l}

xmlNS :: Element -> Maybe NamespaceInfo
xmlNS e = do
   guard (elName e == nsName "ns")
   let i    = fromMaybe ""  $ pAttr "id" e
   let t    = strContent e
   let sub  = isJust (pAttr "subpages" e)
   return NamespaceInfo{nsId=i,nsTitle=t,nsSubpages=sub}

xmlGr :: Element -> Maybe UserGroup
xmlGr e = do
   guard (elName e == nsName "group")
   let nm   = fromMaybe ""  $ pAttr "name" e
   rs <- fmap (mapMaybe xmlRi) (fmap children $ pNode "rights" (children e))
   return UserGroup{ugName=nm,ugRights=rs}
 where
  xmlRi p = do
   guard (elName p == nsName "permission")
   return (strContent e)

xmlIW :: Element -> Maybe InterwikiEntry
xmlIW e = do
   guard (elName e == nsName "iw")
   let pre  = fromMaybe ""  $ pAttr "prefix" e
   let url  = fromMaybe ""  $ pAttr "url" e
   let la   = pAttr "lang" e
   let loc  = isJust (pAttr "local" e)
   let tra  = (pAttr "trans" e >>= \x -> readMb x >>= \ y -> return (y /= (0::Int)))
   return InterwikiEntry{iwPrefix=pre,iwLocal=loc,iwTranscludable=tra,iwUrl=url,iwLanguage=la}

xmlSS :: Element -> Maybe (String,[String])
xmlSS e = do
   guard (elName e == nsName "specialpage")
   let es1 = children e
   nss <- fmap (mapMaybe xmlAS) (fmap children $ pNode "aliases" es1)
   let nm   = fromMaybe ""  $ pAttr "realname" e
   return (nm,nss)
 where
  xmlAS p = do
   guard (elName p == nsName "alias")
   return (strContent e)

xmlSI :: Element -> Maybe SiteInfo
xmlSI e = do
   guard (elName e == nsName "general")
   let ma   = fromMaybe ""  $ pAttr "mainpage" e
   let ba   = fromMaybe ""  $ pAttr "base" e
   let nm   = fromMaybe ""  $ pAttr "sitename" e
   let ge   = fromMaybe ""  $ pAttr "generator" e
   let re   = pAttr "revid" e
   let ca   = pAttr "case" e
   let ri   = pAttr "rights" e
   let ric  = pAttr "rightscode" e
   let la   = pAttr "lang" e
   let enc  = pAttr "fallback8bitEncoding" e
   let wr   = isJust (pAttr "writeapi" e)
   let tz   = pAttr "timezone" e
   let tzo  = pAttr "timeoffset" e >>= readMb
   return SiteInfo
      { siteMainPage = ma
      , siteBase     = ba
      , siteName     = nm
      , siteGenerator  = ge
      , siteLastRevision = re
      , siteCase       = ca
      , siteRightsCode = ric
      , siteRights     = ri
      , siteLang       = la
      , siteFallbackEncoding = enc
      , siteWriteAPI   = wr
      , siteTimezone   = tz
      , siteTZOffset   = tzo
      }
      
xmlStat :: Element -> Maybe SiteStatistics
xmlStat e = do
   guard (elName e == nsName "statistics")
   let pgs  = fromMaybe 0  $ pAttr "pages" e >>= readMb
   let arts = fromMaybe 0  $ pAttr "articles" e >>= readMb
   let views = fromMaybe 0  $ pAttr "views" e >>= readMb
   let edits = fromMaybe 0  $ pAttr "edits" e >>= readMb
   let users = fromMaybe 0  $ pAttr "users" e >>= readMb
   let admins = fromMaybe 0  $ pAttr "admins" e >>= readMb
   let jobs = fromMaybe 0  $ pAttr "jobs" e >>= readMb
   let images = fromMaybe 0  $ pAttr "images" e >>= readMb
   return 
    SiteStatistics
      { siPages    = pgs
      , siArticles = arts
      , siViews    = views
      , siEdits    = edits
      , siImages   = images
      , siUsers    = users
      , siAdmins   = admins
      , siJobs     = jobs
      }
