module Main(main) where

import MediaWiki.API
import MediaWiki.API.Types

import MediaWiki.API.Query.Info
import MediaWiki.API.Query.Info.Import as I
import MediaWiki.API.Query.LangLinks
import MediaWiki.API.Query.LangLinks.Import as LL
import MediaWiki.API.Query.SiteInfo

import System.Environment
import Data.List ( isPrefixOf )

mw_api_url :: String
mw_api_url = "http://en.wikipedia.org/w/"

main :: IO ()
main = do
  ls <- getArgs
  let
   (url,pg) =
    case ls of
      ("WP":y:_) -> (mw_api_url,y)
      (x:y:_) -> (x,y)
      _ -> (mw_api_url, "Main_Page")
  i <- queryInfo url pg
  case I.stringXml i of
    Left x -> putStrLn ("Error getting info for page " ++ pg ++ " error: " ++ show x)
    Right x -> putStrLn ("Page last touched: " ++ headS (map infTouched (infPages x)))
  lns <- getAll url pg [] Nothing
  putStrLn ("The page " ++ pg ++ " is available in other languages:")
  iwMap <- getIWUrlMap url
  let
   getIWUrl (x,y) =
     case filter ((x==).iwPrefix) iwMap of
        (a:_) -> (x,subst y (iwUrl a))
	_ -> (x,"<unknown>")
  let lns1 = map getIWUrl lns
  putStrLn (' ':' ':unlines (map (\ (a,b) -> a ++ "=> " ++ b) lns1))
 where
   subst ss x =
    case spanUntil ("$1" `isPrefixOf`) x of
     (as,_:_:bs) -> as ++ ss ++ subst ss bs
     (as,_) -> as

spanUntil :: ([a] -> Bool) -> [a] -> ([a],[a])
spanUntil _ [] = ([],[])
spanUntil p ls@(x:xs)
 | p ls = ([],ls)
 | otherwise =
     case spanUntil p xs of
       (as,bs) -> (x:as,bs)

getAll :: String -> String -> [[(String,String)]] -> Maybe String -> IO [(String,String)]
getAll url pg acc mbCont = do
--  i <- catch (queryLangPage url pg mbCont) (\ _ -> return "")
  i <- queryLangPage url pg mbCont
  case LL.stringXml i of
   Left{} -> return (concat $ reverse acc)
   Right ll -> do
    let lls = map (\ lli -> (langName lli, lTitle lli)) (concat (map snd (llPages ll)))
    case llContinue ll of
      Nothing -> return $ concat $ reverse (lls:acc)
      Just mb -> do
        getAll url pg (lls:acc) (Just mb)
 where
  lTitle x =
    case langTitle x of
      Nothing -> pg
      Just "" -> pg
      Just xs -> xs

getIWUrlMap :: String -> IO [InterwikiEntry]
getIWUrlMap api_url = do
  mb <- querySiteIWInfo api_url
  case mb of
    Nothing -> return []
    Just si -> return (siInterwiki si)

headS :: [String] -> String
headS    [] = ""
headS (x:_) = x
