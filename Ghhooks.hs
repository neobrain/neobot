{-# LANGUAGE OverloadedStrings #-}

module Ghhooks
    (start) where

import Control.Applicative ((<$>))
import Data.Aeson (eitherDecode, decode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import Network.HTTP.Types
    ( ok200, badRequest400, notFound404, internalServerError500
    , urlDecode, methodPost, hContentType, methodGet
    )
import Network.Wai
    ( requestBody, strictRequestBody, responseLBS, Application
    , rawPathInfo, requestMethod, requestHeaders, responseStream
    )
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Logger (withStdoutLogger, ApacheLogger)

import Control.Monad.Trans.Either (runEitherT)

import Heist
import Heist.Compiled (renderTemplate)
import Control.Lens

import Blaze.ByteString.Builder.Char8 -- (toString)

import qualified Heist.Compiled as C
import Heist.Compiled (renderTemplate)

import qualified Data.Text (pack)

import Github.PostReceive.Types --(Payload)
-- import MyGithubTypes (Payload)

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

foo = "menu" ## testSplice
  where testSplice = do
                       --return $ C.yieldRuntimeText $ do return "<h1>"
                       let stuff = C.yieldRuntimeText $ (do return $ Data.Text.pack ("<div class=\"pure-u-4-24\">\r\n" ++
                                                                                     "    <div id=\"nav\" class=\"pure-u\"> \r\n" ++
                                                                                     "            <a href=\"#\" class=\"pure-menu-heading\">MY STUFF</a>\r\n" ++
                                                                                     "            <div class=\"nav-inner\">\r\n" ++
                                                                                     "                <div class=\"pure-menu\">\r\n" ++
                                                                                     "                    <ul class=\"pure-menu-list\">\r\n" ++
                                                                                     "                        <li class=\"pure-menu-item\"><a href=\"#\" class=\"pure-menu-link\">About</a></li>\r\n" ++
                                                                                     "                        <li class=\"pure-menu-item\"><a href=\"#\" class=\"pure-menu-link\">Blog</a></li>\r\n" ++
                                                                                     "                        <li class=\"pure-menu-item\"><a href=\"#\" class=\"pure-menu-link\">Download</a></li>\r\n" ++
                                                                                     "                        <li class=\"pure-menu-item\"><a href=\"#\" class=\"pure-menu-link\">GitHub</a></li>\r\n" ++
                                                                                     "                        <li class=\"pure-menu-item\"><a href=\"#\" class=\"pure-menu-link\">Contact</a></li>\r\n" ++
                                                                                     "                    </ul>\r\n" ++
                                                                                     "                </div>\r\n" ++
                                                                                     "            </div>\r\n" ++
                                                                                     "        </div>\r\n" ++
                                                                                     "    </div>"))
                       let stuff2 = C.yieldRuntime $ do return ""
                       -- let stuff3 = C.runChildren
                       return $ mappend stuff stuff2 -- stuff2
                       -- C.runChildrenWith foo
                       -- C.runChildren
            -- return $ C.yieldRuntimeText $ do return "</h1>"

heistConfig =
  (set hcNamespace "") $
  (set hcCompiledSplices foo) $
  (set hcLoadTimeSplices defaultLoadTimeSplices) $
  (set hcTemplateLocations [loadTemplates "."]) $
  emptyHeistConfig

app :: ApacheLogger -> M.Map B.ByteString (Payload -> IO ()) -> Application
app aplogger routes req respond
    | method == methodPost = flip (maybe notFound) (M.lookup path routes) $ \cont ->
        case contentType of
            Just "application/json" -> jsonCase cont
            Just "application/x-www-form-urlencoded" -> formCase cont
            _ -> badRequest
    | method == methodGet && path == "/main.css" = respond $ responseStream ok200 [("Content-Type", "text/css")] $ \write flush -> do
                    putStrLn "accessed main.css"
                    fileContents <- readFile "main.css"
                    write $ fromString fileContents
                    flush
    | method == methodGet = respond $ responseStream ok200 [("Content-Type", "text/html")] $ \write flush ->
                do
                    putStrLn $ B.unpack path
                    --let heistConfig = --(set hcCompiledSplices "foo" ## splice)
                    --        set hcLoadTimeSplices defaultLoadTimeSplices $ (set hcTemplateLocations [loadTemplates "."]) $ emptyHeistConfig
                    heistState <- either (error "oops1") id <$> (runEitherT $ initHeist heistConfig)
                    builder <- maybe (error "oops2") fst $ renderTemplate heistState "index"
                    write builder
                    flush
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
        BL.putStrLn bs -- $ B.unpack bs
        --print bs -- $ B.unpack bs
        flip (either internalError) (eitherDecode bs) $ \payload -> cont payload >> ok
    formCase cont = do
        bs <- BL.drop (BL.length "payload=") <$> strictRequestBody req
        flip (either internalError) (eitherDecode $ BL.fromStrict $ urlDecode True $ BL.toStrict bs) $ \payload ->
            cont payload >> ok
