module MediaWiki.API.Query.Categories.Import where

import MediaWiki.API.Types
import MediaWiki.API.Utils
import MediaWiki.API.Query.Categories

import Text.XML.Light.Types

import Control.Monad
import Data.Maybe

stringXml :: String -> Either (String,[{-Error msg-}String]) CategoriesResponse
stringXml s = parseDoc xml s

xml :: Element -> Maybe CategoriesResponse
xml e = do
  guard (elName e == nsName "api")
  let es1 = children e
  q  <- pNode "query" es1
  let es2 = children q
  p  <- pNode "pages" es2
  let ps0 = pNodes "page" $ children p
  let ps1 = catMaybes (map xmlPage ps0)
--  ps <- fmap (mapMaybe xmlPage) (fmap children $ pNode "page" es)
  let cont = pNode "query-continue" es1 >>= xmlContinue "categories" "clcontinue"
  return emptyCategoriesResponse{clPages=ps1,clContinue=cont}

xmlPage :: Element -> Maybe (PageTitle,[PageTitle])
xmlPage e = do
   guard (elName e == nsName "page")
   let es = children e
     -- depending on being 'prop'-constrained, a 'page'
     -- may have a 'categories' child. (Or the page may not belong
     -- to a category at all, I suppose..)
   let cs =
        maybe []
              (\ c -> catMaybes (map xmlCL (pNodes "cl" $ children c)))
              (pNode "categories" es)
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   return (emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}, cs)

xmlCL :: Element -> Maybe PageTitle
xmlCL e = do
   guard (elName e == nsName "cl")
   let ns     = fromMaybe "0" $ pAttr "ns" e
   let tit    = fromMaybe ""  $ pAttr "title" e
   let mbpid  = pAttr "pageid" e
   return emptyPageTitle{pgNS=ns,pgTitle=tit,pgMbId=mbpid}
