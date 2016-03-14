{-# OPTIONS_GHC -cpp -XExistentialQuantification -XDeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module      : MediaWiki.API
-- Description : A Haskell MediaWiki API binding
-- Copyright   : (c) Sigbjorn Finne, 2008
-- License     : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
-- A Haskell MediaWiki API binding.
--
--------------------------------------------------------------------
module MediaWiki.API
       ( module MediaWiki.API
       , URLString
       ) where

import MediaWiki.API.Base
import MediaWiki.API.Types
import MediaWiki.API.Output

import MediaWiki.Util.Fetch as Fetch
import Codec.MIME.Type

import MediaWiki.API.Query.SiteInfo.Import as SI
import MediaWiki.API.Action.Login.Import as Login

import Data.Maybe

import Control.Exception as CE
import Data.Typeable

import MediaWiki.API.Utils
import Text.XML.Light.Types
import Control.Monad

import Data.Text (pack, unpack)

-- | @webGet url req@ issues a GET to a MediaWiki server, appending
-- @api.php?@ followed by the request @req@ to the URL base @url@.
webGet :: URLString -> Request -> IO String
webGet url req = do
  let url_q = url ++ "api.php?" ++ showRequest req
--  print url_q
  readContentsURL url_q

-- | @webGet mbUser url req@ issues a POST to a MediaWiki server, appending
-- @api.php?@ followed by the request @req@ to the URL base @url@.
webPost :: Maybe Fetch.AuthUser -> URLString -> String -> Request -> IO ([(String,String)], String)
webPost mbUser url act req = do
  let url_q  = url ++ "api.php?action="++act
  let pload  = showRequest req
  (_cookiesOut, hs, p) <-
     postContentsURL mbUser
                  url_q
                  [ ("Content-Length", show $ length pload)
		  , ("Content-Type",   unpack $ showMIMEType form_mime_ty)
		  ]
		  [{-no cookies..-}]
		  pload
  return (hs, p)
 where
  form_mime_ty = Application $ pack "x-www-form-urlencoded"

webPostXml :: (String -> Either (String,[String]) a)
           -> Maybe Fetch.AuthUser
           -> URLString
	   -> String
	   -> Request
	   -> IO (Maybe a)
webPostXml p mbUser url act req = do
  (_hs,mb) <- webPost mbUser url act req
  case mb of
    "" -> return Nothing
    ls -> do
     case p ls of
      Left (x,errs) ->
         case parseError ls of
	   Right e -> throwMWError e
	   _ -> putStrLn (x ++ ':':' ':unlines errs) >> return Nothing
      Right x  -> return (Just x)

webGetXml :: (String -> Either (String,[String]) a)
          -> URLString
	  -> Request
	  -> IO (Maybe a)
webGetXml p url req = do
  ls <- webGet url req
  case p ls of
    Left (x,errs) ->
         case parseError ls of
	   Right e -> throwMWError e
	   _ -> putStrLn (x ++ ':':' ':unlines errs) >> return Nothing
    Right x  -> return (Just x)

queryPage :: PageName -> QueryRequest
queryPage pg = emptyQuery{quTitles=[pg]}

mkQueryAction :: APIRequest a => QueryRequest -> a -> Action
mkQueryAction q qr =
  case queryKind qr of
    QProp s -> Query q{quProps=(PropKind s):quProps q} (toReq qr)
    QList s -> Query q{quLists=(ListKind s):quLists q} (toReq qr)
    QMeta s -> Query q{quMetas=(MetaKind s):quMetas q} (toReq qr)
    QGen  s -> Query q{quGenerator=(Just (GeneratorKind s))} (toReq qr)

-- | @loginWiki u usr pass@ logs in to MediaWiki install at @url@ as
-- user @usr@ with password credentials @pass@. Notice that we don't
-- presently allow HTTP Auth to happen in conjunction with the Wiki
-- login.
loginWiki :: URLString -> String -> String -> IO (Maybe LoginResponse)
loginWiki url usr pwd = webPostXml Login.stringXml Nothing url "login" req
  where
   req = emptyXmlRequest (Login (emptyLogin usr pwd))

queryInfo :: URLString -> PageName -> IO String
queryInfo url pgName = webGet url req
  where
   req = emptyXmlRequest (mkQueryAction (queryPage pgName) infoRequest)

querySiteIWInfo :: URLString -> IO (Maybe SiteInfoResponse)
querySiteIWInfo url = webGetXml SI.stringXml url req
 where
  req = emptyXmlRequest
                 (mkQueryAction (queryPage "XP")
		                siteInfoRequest{siProp=["interwikimap"]})

queryLangPage :: URLString -> PageName -> Maybe String -> IO String
queryLangPage url pgName mb = webGet url req
  where
   req = emptyXmlRequest
                 (mkQueryAction (queryPage pgName)
		                langLinksRequest{llContinueFrom=mb})


parseError :: String -> Either (String,[{-Error msg-}String]) MediaWikiError
parseError s = parseDoc xmlError s

xmlError :: Element -> Maybe MediaWikiError
xmlError e = do
  guard (elName e == nsName "api")
  let es1 = children e
  p  <- pNode "error" es1
  return mwError{ mwErrorCode = fromMaybe "" $ pAttr "code" p
                , mwErrorInfo = fromMaybe "" $ pAttr "info" p
		}

-- MW exceptions/errors:

data MediaWikiError
 = MediaWikiError
     { mwErrorCode :: String
     , mwErrorInfo :: String
     } deriving ( Typeable )

mwError :: MediaWikiError
mwError = MediaWikiError{mwErrorCode="",mwErrorInfo=""}

data SomeMWException = forall e . Exception e => SomeMWException e
    deriving Typeable

instance Show SomeMWException where
    show (SomeMWException e) = show e

instance Exception SomeMWException

mwToException :: Exception e => e -> SomeException
mwToException = toException . SomeMWException

mwFromException :: Exception e => SomeException -> Maybe e
mwFromException x = do
    SomeMWException a <- fromException x
    cast a

instance Exception MediaWikiError where
  toException = mwToException
  fromException = mwFromException

throwMWError :: MediaWikiError -> IO a
throwMWError e = throwIO e

catchMW :: IO a -> (MediaWikiError -> IO a) -> IO a
catchMW f hdlr =
  CE.catch f
           (\ e1 -> hdlr e1)

handleMW :: (MediaWikiError -> IO a) -> IO a -> IO a
handleMW h e = catchMW e h

tryMW :: IO a -> IO (Either MediaWikiError a)
tryMW f = handleMW (\ x -> return (Left x)) (f >>= return.Right)


instance Show MediaWikiError where
  show x = unlines (
   [ "MediaWiki error:"
   , ""
   , " Code: " ++ mwErrorCode x
   , " Info: " ++ mwErrorInfo x
   ])
