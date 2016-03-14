{-
  Querying for back/in links to a given page.

  foo$ path/to/linksTo "Haskell"
  ...
  foo$
-}
module Main(main) where

import MediaWiki.API.Base
import MediaWiki.API.Types ( PageTitle(..) )
import MediaWiki.API

import MediaWiki.API.Query.BackLinks.Import as Back

import Util.GetOpts

-- begin option handling
data Options
 = Options
    { optWiki :: String
    , optUser :: Maybe String
    , optPass :: Maybe String
    , optPage :: Maybe String
    }

nullOptions :: Options
nullOptions = Options
    { optWiki  = "http://en.wikipedia.org/w/"
    , optUser  = Nothing
    , optPass  = Nothing
    , optPage  = Nothing
    }

option_descr :: [OptDescr (Options -> Options)]
option_descr =
  [ Option ['u'] ["user"]
           (ReqArg (\ x o -> o{optUser=Just x}) "USER")
	   "Wiki user name to login as"
  , Option ['p'] ["pass"]
           (ReqArg (\ x o -> o{optPass=Just x}) "PASSWORD")
	   "user's password credentials to supply if logging in"
  , Option ['w'] ["wiki"]
           (ReqArg (\ x o -> o{optWiki=x}) "WIKI")
	   "the Wiki to access"
  , Option ['P'] ["page"]
           (ReqArg (\ x o -> o{optPage=Just x}) "PAGE")
	   "the page title to list category pages for"
  ]

-- end option handling

queryPageInLinks :: URLString -> String -> IO [PageTitle]
queryPageInLinks url pgName = queryInLinks emptyBackLinksRequest{blTitle=Just pgName} []
 where
  queryInLinks bReq acc = do
    let req = emptyXmlRequest (mkQueryAction (queryPage pgName) bReq)
    mb <- webGetXml Back.stringXml url req
    case mb of
      Nothing -> fail ("Failed to fetch page" ++ pgName ++ " from " ++ url)
      Just c  -> do
        let acc' = acc ++ blLinks c
        case blContinue c of
          Nothing -> return acc'
	  Just x  -> queryInLinks bReq{blContinueFrom=Just x} acc'

main :: IO ()
main = do
  (opts, fs) <- processOptions option_descr nullOptions
  let url = optWiki opts
  case mbCons (optPage opts) fs of
    [] -> return ()
    xs -> mapM_ (linksToPage url) xs

linksToPage :: URLString -> String -> IO ()
linksToPage url pgName = do
  ps <- queryPageInLinks url pgName
  putStrLn ("Page " ++ show pgName ++ " has the following backlinks: ")
  mapM_ (putStrLn.toTitle) ps
 where
  toTitle pg = ' ':pgTitle pg

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing xs = xs
mbCons (Just x) xs = x:xs
