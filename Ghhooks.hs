{-# LANGUAGE OverloadedStrings #-}

module Ghhooks
    (start) where

import Control.Applicative ((<$>))
import Data.Aeson (eitherDecode, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import Network.HTTP.Types
    ( ok200, badRequest400, notFound404, internalServerError500
    , urlDecode, methodPost, hContentType
    )
import Network.Wai
    ( requestBody, strictRequestBody, responseLBS, Application
    , rawPathInfo, requestMethod, requestHeaders
    )
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Logger (withStdoutLogger, ApacheLogger)

import Github.PostReceive.Types --(Payload)
-- import MyGithubTypes (Payload)

myprint :: Payload -> IO ()
myprint content = case content of
    Push pe -> print $ "Hi!" ++ show pe
    _ -> print "Unknown content!"

--main :: IO ()
--main = start 4567 $ M.fromList [("/stuff", myprint)]

start :: Port -> M.Map B.ByteString (Payload -> IO ()) -> IO ()
start port routes = do
    putStrLn startingMessage
    withStdoutLogger $ run port . flip app routes
  where
    startingMessage = concat
        [ "github-post-receive listening on port "
        , show port
        , " with path "
        , show $ M.keys routes
        ]

app :: ApacheLogger -> M.Map B.ByteString (Payload -> IO ()) -> Application
app aplogger routes req respond
    | method == methodPost = flip (maybe notFound) (M.lookup path routes) $ \cont ->
        case contentType of
            Just "application/json" -> jsonCase cont
            Just "application/x-www-form-urlencoded" -> formCase cont
            _ -> badRequest
    | otherwise = notFound
  where
    path = rawPathInfo req
    method = requestMethod req
    contentType = lookup hContentType $ requestHeaders req
    res status = aplogger req status Nothing >> respond (responseLBS status [] BL.empty)
    notFound = res notFound404
    badRequest = res badRequest400
    internalError reason = putStrLn ("Reporting internal error: " ++ reason) >> res internalServerError500
    ok = res ok200
    jsonCase cont = do
        bs <- strictRequestBody req
        --bs <- requestBody req
        --putStrLn $ show bs
        --BL.putStrLn bs -- $ B.unpack bs
        print bs -- $ B.unpack bs
        flip (either internalError) (eitherDecode bs) $ \payload -> cont payload >> ok
    formCase cont = do
        bs <- BL.drop (BL.length "payload=") <$> strictRequestBody req
        flip (either internalError) (eitherDecode $ BL.fromStrict $ urlDecode True $ BL.toStrict bs) $ \payload ->
            cont payload >> ok
