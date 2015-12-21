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

-- atomically $ newTChan :: IO TChan

main = do
    maybeConfig <- Config.readConfig "config.json"
    case maybeConfig of
        Nothing -> putStrLn "Couldn't parse config!"
        Just config -> do
                         let -- githubChans = HM.fromList $ map (,(atomically newTChan)) $ networks_channels_list
                             networks_channels_list = concat $ map (\network -> map (network,) (Config.networkChannels network)) (Config.configNetworks config)
                             watching_channels = filter (watches_some_repo . snd) networks_channels_list
                             watches_some_repo = null . Config.channelWatchedGithubRepos

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
                    Github.Push         event -> sequence_ $ map (\commit -> notify_commit network event commit h) (Github.pushEventCommits event)
                    Github.IssueComment event -> notify_issue_comment network event h
                    Github.Issue        event -> notify_issue network event h
                    _                  -> print "Received payload, but not sure what to do with it!"
                Nothing -> do
                    return ()
        evalraw config network h s
    putStrLn s
    where
        forever a = do a; forever a

        ping x    = "PING :" `isPrefixOf` x
        pong x    = write h "PONG" (':' : drop 6 x)

lookup_channel_in_network :: Config.Network -> String -> Maybe Config.Channel
lookup_channel_in_network network channel = find ((channel==) . T.unpack . Config.channelName) $ Config.networkChannels network

lookup_channel_in_config :: Config.Config -> String -> String -> Maybe Config.Channel
lookup_channel_in_config config network channel =
    maybe Nothing channel_match network_match
    where network_match = lookup_network_in_config config network
          channel_match network = lookup_channel_in_network network channel

lookup_network_in_config :: Config.Config -> String -> Maybe Config.Network
lookup_network_in_config config network = find ((network==) . T.unpack . Config.networkName) $ Config.configNetworks config

-- Get a list of all channels in the given network which observe the given GitHub repository
getChannelsObservingRepo :: Config.Network -> Github.Repository -> [Config.Channel]
getChannelsObservingRepo network repo = filter channel_observing all_channels
    where
        all_channels = Config.networkChannels network
        -- Assume the repository name is build like "owner/name"
        repo_full_name = break (=='/') (T.unpack $ Github.repoFullName repo)
        reference_watched_repo = Config.WatchedRepo
                                 { Config.watchedRepoOwner = T.pack $ fst repo_full_name
                                 , Config.watchedRepoName = T.pack $ drop 1 $ snd repo_full_name }
        channel_observing = elem reference_watched_repo . Config.channelWatchedGithubRepos

-- Notify the IssueEvent to all interested channels on the given network
notify_issue :: Config.Network -> Github.IssueEvent -> Handle -> IO ()
notify_issue network event h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ sender ++ " " ++ action ++ " issue #" ++ issue_id ++ ": \"" ++ title ++ "\" --- " ++ url
        repo = Github.issueEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueEventIssue event
        title = T.unpack $ Github.issueTitle issue
        issue_id = show $ Github.issueNumber issue
        comment = T.unpack $ Github.issueBody issue
        sender_user = Github.issueEventSender event
        sender = T.unpack $ Github.userLogin $ sender_user
        url = B.unpack $ Github.issueHtmlUrl issue
        action = T.unpack $ Github.issueEventAction event

-- Notify the IssueCommentEvent to all interested channels on the given network
notify_issue_comment :: Config.Network -> Github.IssueCommentEvent -> Handle -> IO ()
notify_issue_comment network event h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ "\x0312\x02" ++ author ++ "\x0F commented on issue #" ++ issue_id ++ ": \"" ++ body ++ "\" --- " ++ url
        repo = Github.issueCommentEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueCommentEventIssue event
        issue_id = show $ Github.issueNumber issue
        comment = Github.issueCommentEventComment event
        author_user = Github.commentUser comment
        author = T.unpack $ Github.userLogin $ Github.commentUser comment
        body = shorten_to_length 100 $ takeWhile (/= '\r') $ takeWhile (/= '\n') $ T.unpack $ Github.commentBody comment -- ugly way to make sure both \r and \n are recognized as newline characters
        url = B.unpack $ Github.commentHtmlUrl comment
        shorten_to_length n str = if length str <= n then str else (take (n - 6) str) ++ " [...]"

-- Notify the PushEvent's commit to all interested channels on the given network
notify_commit :: Config.Network -> Github.PushEvent -> Github.Commit -> Handle -> IO ()
notify_commit network event commit h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ "New patch by " ++ author ++ ": " ++ message ++ " (" ++ url ++ ")"
        repo = Github.pushEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        author = T.unpack $ Github.simpleUserName $ Github.commitCommitter commit
        message = T.unpack $ Github.commitMessage commit
        url = B.unpack $ Github.commitUrl commit

-- Handle -> rawmessage
evalraw :: Config.Config -> Config.Network -> Handle -> String -> IO ()
evalraw  config network h (':' : s) = case command of
    _ : x : chan_name : tail | (x=="PRIVMSG") -> maybe (return ()) handleMessage (lookup_channel_in_network network chan_name)
                             | otherwise -> printf "Unknown command %s\n" x
                                  where handleMessage chan = evalPrivMsg config network chan h "TODOSENDERNAME" $ drop 1 $ unwords tail
    otherwise -> printf $ "INVALID COMMAND \n" ++ (show command)
    where command = words s
evalraw _ _ _ s = printf "Received unknown message %s\n" s

-- Config -> Network -> Channel -> Handle -> sender -> message
evalPrivMsg :: Config.Config -> Config.Network -> Config.Channel -> Handle -> String -> String -> IO ()
-- regular commands
evalPrivMsg _ _ chan h _  "!quit"    = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
--evalPrivMsg _ _ chan h _ x | "!quit " `isPrefixOf` x   = write h ("QUIT" ": Exiting (" ++ (drop 6 x) ++ ")") >> exitWith ExitSuccess
evalPrivMsg _ _ chan h _ "!help"    = privmsg h chan "Supported commands: !about, !help, !issue N, !say, !love, !xkcd"
evalPrivMsg _ _ chan h _ "!about"   = privmsg h chan "I'm indeed really awesome! Learn more about me in #neobot."
evalPrivMsg _ _ chan h _ "!love"    = privmsg h chan "Haskell is love. Haskell is life."
evalPrivMsg _ _ chan h _ x | "!xkcd " `isPrefixOf` x = privmsg h chan $ "https://xkcd.com/" ++ (drop 6 x) -- TODO: Use https://xkcd.com/json.html to print the title!
evalPrivMsg _ _    _ h _ x | "!join " `isPrefixOf` x = write h "JOIN" (drop 6 x)

-- !say with and without target channel
evalPrivMsg _ network source_chan h _ x | "!say #" `isPrefixOf` x = case maybeChan of
    Nothing -> privmsg h source_chan $ "I don't know the channel " ++ chan_name ++ "!"
    Just chan -> privmsg h chan message
    where chan_name = head $ tail $ words x
          maybeChan = lookup_channel_in_network network chan_name
          message = unwords $ tail $ tail $ words x
evalPrivMsg _ _ chan h _ x | "!say " `isPrefixOf` x = privmsg h chan (drop 5 x)

evalPrivMsg config network chan h _ x | "!issue " `isPrefixOf` x = getissue config network chan h (drop 7 x) -- TODO: Change getissue to return a printable value instead of making it write stuff
-- TODO: !kpop: Post a random video link from a list
-- keyword recognition
evalPrivMsg _ _ chan h _ x | isGreeting x && (Config.channelReplyToGreetings chan) = privmsg h chan $ "Hi! Welcome to " ++ (T.unpack $ Config.channelName chan) ++ ". Do you have a question on your mind? I'm just a bot, but lots of real people are hanging around here and may be able to help. Make sure to stick around for more than a few minutes to give them a chance to reply, though!"
evalPrivMsg _ _ chan h _ x | ("msvcp" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Got an MSVCP dll error? Download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 ."
evalPrivMsg _ _ chan h _ x | ("0x0000007b" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "Got a 0x0000007b error when starting Citra? Don't download random .dll files from the internet! Undo your local modifications, and download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 instead."
-- TODO: Recognize /pull/ as part of a github url
-- possibly offensive stuff
evalPrivMsg _ _ chan h _ x | ("emucr" `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "Please don't support emucr by downloading our (or any, really) software from emucr."
evalPrivMsg _ _ chan h _ x | ("pokemon" `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "No, Pokémon does not run, yet."
evalPrivMsg _ _ chan h _ x | (" rom " `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) && (" download" `isInfixOf` (map Char.toLower x)) = privmsg h chan $ "We don't support piracy here."
evalPrivMsg _ _ chan h _ x | (" support " `isInfixOf` (map Char.toLower x)) && (" android " `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "Android support is far from being a priority, currently."
evalPrivMsg _ _ chan h _ x | ("3dmoo" `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "3dmoo, hah. Good one!"
evalPrivMsg _ _ chan h _ x | ("tronds" `isInfixOf` (map Char.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "TronDS is a very successfull 3DS emulator, indeed. Happy Opposite Day!"
evalPrivMsg _ _ chan h _ x = printf "> %s" $ "Ignoring message \"" ++ x ++ "\" from channel " ++ (T.unpack $ Config.channelName chan) ++ "\n"

isGreeting :: String -> Bool
isGreeting s = any (`isPrefixOf` (map Char.toLower s)) ["hello", "hey", "hi", "helo", "helllo", "anyone here", "anybody here", "nobody", "help"]

myreadMaybe :: Read a => String -> Maybe a
myreadMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

getissue :: Config.Config -> Config.Network -> Config.Channel -> Handle -> String -> IO ()
getissue config network channel h x =
    case maybeIssueNum of
        Nothing -> privmsg h channel $ "That is not a valid issue number."
        Just issueNum ->
            do
                possibleIssue <- GHAPI.issue' auth repo_owner repo_name issueNum
                case possibleIssue of
                    (Left err) -> privmsg h channel $ "Error: " ++ show err
                    (Right issue) -> privmsg h channel $ "Issue " ++ show issueNum ++ ", reported by " ++ (GHAPI.githubOwnerLogin (GHAPI.issueUser issue)) ++ ": " ++ (GHAPI.issueTitle issue)

    where maybeIssueNum = myreadMaybe x :: Maybe Int
          repo_owner = T.unpack $ Config.watchedRepoOwner $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          repo_name = T.unpack $ Config.watchedRepoName $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          auth = Just $ GHAPI.GithubOAuth $ T.unpack $ Config.configAuthToken config

-- TODO: Change to "Channel -> Handle -> String -> IO ()" for consistency!
privmsg :: Handle -> Config.Channel -> String -> IO ()
privmsg h chan s = write h "PRIVMSG" (chan_name ++ " :" ++ s)
    where chan_name = T.unpack $ Config.channelName chan
