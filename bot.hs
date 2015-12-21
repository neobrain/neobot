{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Exception
import Text.Printf

import System.Environment

import Github.Auth as GHAPI
import Github.Issues as GHAPI

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Char as Char

import Data.Maybe (isJust, fromJust)

import Control.Monad (forM_, void)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM.TChan

import Ghhooks

import qualified Config

import qualified Github.PostReceive as Github
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Char8 as B

type GithubChanMap = HM.HashMap (Config.Network, Config.Channel) (TChan Github.Payload)

-- githubThread :: GithubChanMap -> IO ()
githubThread :: TChan Github.Payload -> IO ()
githubThread chan = start 4567 $ M.fromList [("/stuff", (onEvent chan))]

-- onEvent :: GithubChanMap -> Github.Payload -> IO ()
onEvent :: TChan Github.Payload -> Github.Payload -> IO ()
-- onEvent chan_map event = do
onEvent tchan event = do
    print "Writing githubChan.. "
    -- forM_ (HM.elems chan_map) (\tchan -> atomically $ writeTChan tchan event)
    atomically $ writeTChan tchan event
    print "Wrote message!"

{-
forever $ do
    print "Writing githubChan.. "
    atomically $ writeTChan chan "Hi there!"
    print "Wrote message!"
    threadDelay 1000000 -- 0.1 second
    where
        forever a = do a; forever a
-}

-- atomically $ newTChan :: IO TChan

main = do
    maybeConfig <- Config.readConfig "config.json"
    case maybeConfig of
        Nothing -> putStrLn "Couldn't parse config!"
        Just config -> do
                         let -- githubChans = HM.fromList $ map (,(atomically newTChan)) $ networks_channels_list
                             networks_channels_list = concat $ map (\network -> map (network,) (Config.networkChannels network)) (Config.configNetworks config)
                             watching_channels = filter (watches_some_repo . snd) networks_channels_list
                             watches_some_repo = \channel -> all isJust [Config.channelWatchedGithubRepoOwner channel, Config.channelWatchedGithubRepo channel]

                         --githubChans <- do
                         --           -- tchans <- sequence $ repeat $ atomically $ newTChan
                         --           -- return $ HM.fromList $ zip networks_channels_list (repeat $ atomically $ newTChan)
                         --           return $ atomically $ HM.fromList $ zip networks_channels_list (repeat $ newTChan)
                         githubChan <- atomically $ newTChan
                         -- forkIO $ githubThread githubChans -- TODO: This should be shared across all network threads!
                         forkIO $ githubThread githubChan -- TODO: This should be shared across all network threads!

                         -- TODO: Spawn gitHubThread
                         void $ mapConcurrently (\network -> do
                            let
                                server = T.unpack $ Config.networkServer network
                                port = Config.networkPort network
                                nick = T.unpack $ Config.networkNick network
                                channels = Config.networkChannels network
                                watching_channels = filter (watches_some_repo) channels
                            ----githubChan <- atomically $ newTChan
                            --ircReadChan <- atomically $ newTChan
                            --ircWriteChan <- atomically $ newTChan
                            ----forkIO $ githubThread githubChan -- TODO: This should be shared across all network threads!
                            --forkIO $ ircThread ircReadChan ircWriteChan
                            h <- connectTo server (PortNumber (fromIntegral port))
                            hSetBuffering h NoBuffering
                            write h "NICK" nick
                            write h "USER" (nick ++ " 0 * :tutorial bot")
                            mapM_ (\chan -> write h "JOIN" $ T.unpack $ Config.channelName chan) channels
                            -- listen config githubChans h
                            listen config network githubChan h
                            ) (Config.configNetworks config)

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

-- listen :: Config.Config -> GithubChanMap -> Handle -> IO ()
-- listen config tchan_map h = forever $ do
listen :: Config.Config -> Config.Network -> TChan Github.Payload -> Handle -> IO ()
listen config network tchan h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s
    then pong s
    else do
        print $ "Reading message.."
        do
            maybePayload <- atomically $ tryReadTChan tchan
            case maybePayload of
                Just payload -> case payload of
                    --Github.Push event -> notify_commit event (head (Github.pushEventCommits event)) h default_chan
                    Github.Push         event -> sequence_ $ map (\commit -> notify_commit event commit h) (Github.pushEventCommits event)
                    Github.IssueComment event -> notify_issue_comment event h
                    Github.Issue        event -> notify_issue event h
                    _                  -> print "Received payload, but not sure what to do with it!"
                Nothing -> do
                    return ()
        evalraw config network h s
    putStrLn s
    where
        forever a = do a; forever a

        ping x    = "PING :" `isPrefixOf` x
        pong x    = write h "PONG" (':' : drop 6 x)

notify_issue :: Github.IssueEvent -> Handle -> IO ()
notify_issue event h = privmsg h actual_chan $ sender ++ " " ++ action ++ " issue #" ++ issue_id ++ ": \"" ++ title ++ "\" --- " ++ url
    where
        actual_chan = repository_id_to_chan $ Github.repoId $ Github.issueEventRepository event
        issue = Github.issueEventIssue event
        title = T.unpack $ Github.issueTitle issue
        issue_id = show $ Github.issueNumber issue
        comment = T.unpack $ Github.issueBody issue
        sender_user = Github.issueEventSender event
        sender = T.unpack $ Github.userLogin $ sender_user
        url = B.unpack $ Github.issueHtmlUrl issue
        action = T.unpack $ Github.issueEventAction event

repository_id_to_chan :: Int -> String
repository_id_to_chan 35845931 = "#wiiu-emu"
repository_id_to_chan 48068487 = "#neobot"
repository_id_to_chan _ = "#citra"

lookup_channel_in_network :: Config.Config -> Config.Network -> String -> Maybe Config.Channel
lookup_channel_in_network config network channel = find ((channel==) . T.unpack . Config.channelName) $ Config.networkChannels network

lookup_channel_in_config :: Config.Config -> String -> String -> Maybe Config.Channel
lookup_channel_in_config config network channel =
    maybe Nothing channel_match network_match
    where network_match = lookup_network_in_config config network
          channel_match network = lookup_channel_in_network config network channel

lookup_network_in_config :: Config.Config -> String -> Maybe Config.Network
lookup_network_in_config config network = find ((network==) . T.unpack . Config.networkName) $ Config.configNetworks config

notify_issue_comment :: Github.IssueCommentEvent -> Handle -> IO ()
notify_issue_comment event h = privmsg h actual_chan $ "New comment on issue #" ++ issue_id ++ " by " ++ author ++ ": \"" ++ body ++ "\" --- " ++ url
    where
        issue = Github.issueCommentEventIssue event
        actual_chan = repository_id_to_chan $ Github.repoId $ Github.issueCommentEventRepository event
        issue_id = show $ Github.issueNumber issue
        comment = Github.issueCommentEventComment event
        author_user = Github.commentUser comment
        author = T.unpack $ Github.userLogin $ Github.commentUser comment
        body = take 100 $ head $ lines $ T.unpack $ Github.commentBody comment
        url = B.unpack $ Github.commentHtmlUrl comment

notify_commit :: Github.PushEvent -> Github.Commit -> Handle -> IO ()
notify_commit pushevent commit h = privmsg h actual_chan $ "New patch by " ++ author ++ ": " ++ message ++ " (" ++ url ++ ")"
    where
        actual_chan = repository_id_to_chan $ Github.repoId $ Github.pushEventRepository pushevent
        author = T.unpack $ Github.simpleUserName $ Github.commitCommitter commit
        message = T.unpack $ Github.commitMessage commit
        url = B.unpack $ Github.commitUrl commit

-- Handle -> rawmessage
evalraw :: Config.Config -> Config.Network -> Handle -> String -> IO ()
evalraw  config network h (':' : s) = case command of 
    _ : x : chan : tail | (x=="PRIVMSG") -> evalPrivMsg config network h chan $ drop 1 $ unwords tail --clean s -- unwords tail -- clean s
                        | otherwise -> printf "Unknown command %s\n" x
    otherwise -> printf $ "INVALID COMMAND \n" ++ (show command)
    where command = words s
evalraw _ _ _ s = printf "Received unknown message %s\n" s

-- Handle -> channel -> message
evalPrivMsg :: Config.Config -> Config.Network -> Handle -> String -> String -> IO ()
-- regular commands
evalPrivMsg _ _ h _  "!quit"    = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
--evalPrivMsg _ _ h _ x | "!quit " `isPrefixOf` x   = write h ("QUIT" ": Exiting (" ++ (drop 6 x) ++ ")") >> exitWith ExitSuccess
evalPrivMsg _ _ h chan  "!help"    = privmsg h chan "Supported commands: !about, !help, !issue N, !say, !love, !xkcd"
evalPrivMsg _ _ h chan  "!about"   = privmsg h chan "I'm indeed really awesome! Learn more about me in #neobot."
evalPrivMsg _ _ h chan  "!love"    = privmsg h chan "Haskell is love. Haskell is life."
evalPrivMsg _ _ h chan x | "!xkcd " `isPrefixOf` x = privmsg h chan $ "https://xkcd.com/" ++ (drop 6 x) -- TODO: Use https://xkcd.com/json.html to print the title!
evalPrivMsg _ _ h _ x | "!join " `isPrefixOf` x = write h "JOIN" (drop 6 x)
evalPrivMsg _ _ h chan x | "!say " `isPrefixOf` x = privmsg h chan (drop 5 x)
evalPrivMsg config network h chan x | "!issue " `isPrefixOf` x =
    case maybeChannel of
        Nothing -> return ()
        Just channel -> getissue config network channel h (drop 7 x) -- TODO: Change getissue to return a printable value instead of making it write stuff
    where maybeChannel = lookup_channel_in_network config network chan
-- TODO: !kpop: Post a random video link from a list
-- keyword recognition
evalPrivMsg _ _ h chan x | isGreeting x = privmsg h chan $ "Hi! Welcome to " ++ chan ++ ". Do you have a question on your mind? I'm just a bot, but lots of real people are hanging around here and may be able to help. Make sure to stick around for more than a few minutes to give them a chance to reply, though!"
evalPrivMsg _ _ h chan x | ("msvcp" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Got an MSVCP dll error? Download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 ."
evalPrivMsg _ _ h chan x | ("0x0000007b" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Got a 0x0000007b error when starting Citra? Don't download random .dll files from the internet! Undo your local modifications, and download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 instead."
-- TODO: Recognize /pull/ as part of a github url
-- possibly offensive stuff
evalPrivMsg _ _ h chan x | ("emucr" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Please don't support emucr by downloading our (or any, really) software from emucr."
evalPrivMsg _ _ h chan x | ("pokemon" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "No, Pokémon does not run, yet."
evalPrivMsg _ _ h chan x | (" rom " `isInfixOf` (map Char.toLower x)) && (" download" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "We don't support piracy here."
evalPrivMsg _ _ h chan x | (" support " `isInfixOf` (map Char.toLower x)) && (" android " `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Android support is far from being a priority, currently."
evalPrivMsg _ _ h chan x | ("3dmoo" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "3dmoo, hah. Good one!"
evalPrivMsg _ _ h chan x | ("tronds" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "TronDS is a very successfull 3DS emulator, indeed. Happy Opposite Day!"
evalPrivMsg _ _ h chan x = printf "> %s" $ "Received \"" ++ x ++ "\" from channel " ++ chan ++ "\n"

isGreeting :: String -> Bool
isGreeting s = any (`isPrefixOf` (map Char.toLower s)) ["hello", "hey", "hi", "helo", "helllo", "anyone here", "anybody here", "nobody", "help"]

-- Reduces raw IRC message to the actual message sent
-- TODO: This assumes the core message contains no ":"...
--clean :: String -> String
--clean = drop 1 . dropWhile (/=':') . drop 1

myreadMaybe :: Read a => String -> Maybe a
myreadMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

getissue :: Config.Config -> Config.Network -> Config.Channel -> Handle -> String -> IO ()
getissue config network channel h x =
    let channel_name = T.unpack $ Config.channelName channel
    in case maybeIssueNum of
        Nothing -> privmsg h channel_name $ "That is not a valid issue number."
        Just issueNum ->
            do
                possibleIssue <- GHAPI.issue' auth repo_owner repo_name issueNum
                case possibleIssue of
                    (Left err) -> privmsg h channel_name $ "Error: " ++ show err
                    (Right issue) -> privmsg h channel_name $ "Issue " ++ show issueNum ++ ", reported by " ++ (GHAPI.githubOwnerLogin (GHAPI.issueUser issue)) ++ ": " ++ (GHAPI.issueTitle issue)

    where maybeIssueNum = myreadMaybe x :: Maybe Int
          channel_name = Config.channelName
          repo_owner = T.unpack $ fromJust $ Config.channelWatchedGithubRepoOwner channel -- TODO: This is unsafe!
          repo_name = T.unpack $ fromJust $ Config.channelWatchedGithubRepo channel -- TODO: This is unsafe!
          auth = Just $ GHAPI.GithubOAuth $ T.unpack $ Config.configAuthToken config


privmsg :: Handle -> String -> String -> IO ()
privmsg h chan s = write h "PRIVMSG" (chan ++ " :" ++ s)
