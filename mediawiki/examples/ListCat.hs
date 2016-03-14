{-
  Example of how to query for category members, demonstrating
  the modules, types and steps used to interact with the API:

  foo$ listCat "Functional programming"
  ...
  foo$
-}
module Main(main) where

import MediaWiki.API.Base
import MediaWiki.API.Types ( PageTitle(..) )
import MediaWiki.API
import MediaWiki.API.Query.CategoryMembers.Import as CatMem

import MediaWiki.API.Query.Categories.Import as Cats

import Util.GetOpts
import Data.List

-- begin option handling
data Options
 = Options
    { optWiki    :: String
    , optUser    :: Maybe String
    , optPass    :: Maybe String
    , optCat     :: Maybe String
    , optPage    :: Maybe String
    }

nullOptions :: Options
nullOptions = Options
    { optWiki    = "http://en.wikipedia.org/w/"
    , optUser    = Nothing
    , optPass    = Nothing
    , optCat     = Nothing
    , optPage    = Nothing
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
  , Option ['c'] ["cat"]
           (ReqArg (\ x o -> o{optCat=Just x}) "CAT")
	   "the Wiki category to query for"
  , Option ['P'] ["page"]
           (ReqArg (\ x o -> o{optPage=Just x}) "PAGE")
	   "the page title to list category pages for"
  ]

-- end option handling

queryCat :: URLString -> String -> IO [PageTitle]
queryCat url catName = queryCat' url catName' req []
 where
  catName'
    | "Category:" `isPrefixOf` catName = wikify catName
    | otherwise = "Category:"++wikify catName

  req = categoryMembersRequest{cmTitle=Just catName'}

  wikify "" = ""
  wikify (' ':xs) = '_':wikify xs
  wikify (x:xs) = x : wikify xs


queryCat' :: URLString -> String -> CategoryMembersRequest -> [PageTitle] -> IO [PageTitle]
queryCat' url catName cReq acc = do
  let req = emptyXmlRequest (mkQueryAction (queryPage catName) cReq)
  mb <- webGetXml CatMem.stringXml url req
  case mb of
    Nothing -> fail ("Failed to fetch pages for category " ++ catName ++ " from " ++ url)
    Just c  -> do
      let acc' = (acc ++ cmPages c)
      case cmContinue c of
        Nothing -> return acc'
	Just x  -> queryCat' url catName cReq{cmContinueFrom=Just x} acc'

queryPageCats :: URLString -> String -> IO [PageTitle]
queryPageCats url pgName = queryPageCats' emptyCategoriesRequest []
 where
  queryPageCats' cReq acc = do
    let req = emptyXmlRequest (mkQueryAction (queryPage pgName) cReq)
    mb <- webGetXml Cats.stringXml url req
    case mb of
      Nothing -> fail ("Failed to fetch page" ++ pgName ++ " from " ++ url)
      Just c  -> do
        let acc' = (acc ++ clPages c)
        case clContinue c of
          Nothing -> return (concatMap snd acc')
	  Just x  -> queryPageCats' cReq{clContinueFrom=Just x} acc'

main :: IO ()
main = do
  (opts, fs) <- processOptions option_descr nullOptions
  let url = optWiki opts
    -- not needed here, but left in to show how to login to a Wiki.
  case (optUser opts, optPass opts) of
    (Just u, Just p) -> do
       x <- loginWiki url u p
       case x of
         Nothing -> putStrLn ("Unable to login to: " ++ url)
         Just lg -> print (lgSuccess lg)
    _ -> return ()
  case mbCons (optCat opts) fs of
    [] -> putStrLn "No categories specified."
    xs -> mapM_ (showCat url) xs
  case mbCons (optPage opts) fs of
    [] -> return ()
    xs -> mapM_ (showPages url) xs

showCat :: URLString -> String -> IO ()
showCat url cat = do
  ps <- queryCat url cat
  putStrLn ("Members of category `" ++ cat ++ "'")
  mapM_ (putStrLn.toTitle) ps
 where
  toTitle pg = ' ':pgTitle pg

showPages :: URLString -> String -> IO ()
showPages url pgName = do
  ps <- queryPageCats url pgName
  putStrLn ("Page " ++ show pgName ++ " is a member of the following categories: ")
  mapM_ (putStrLn.toTitle) ps
 where
  toTitle pg = ' ':pgTitle pg

mbCons :: Maybe a -> [a] -> [a]
mbCons Nothing xs = xs
mbCons (Just x) xs = x:xs

