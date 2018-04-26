{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-} -- We need this to pattern match Text, currently..

import Data.List
import Network
import System.IO
import System.IO.Error (isEOFError, catchIOError)
import System.Exit
import Control.Arrow
import Control.Exception
import Text.Printf

import System.Environment

import Github.Auth as GHAPI
import Github.Issues as GHAPI
import Github.Issues.Comments as GHAPI

import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException(DecodeError))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Char as Char

import qualified BasicPrelude as T (show)

import Data.Maybe (isJust, fromJust)

import Control.Monad (forM_, forM, void, when, unless)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently, wait, async, race)
import Control.Concurrent.STM.TChan

import qualified Network.IRC.Conduit as IRC

import Ghhooks

import qualified Config

import qualified Github.PostReceive as Github
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import Data.List
import Data.List.Split

import qualified System.Random as R

import Network.HTTP.Conduit

import Text.Regex.Posix

-- Mediawiki stuff for querying wikis
import qualified MediaWiki.API.Base as MW
import qualified MediaWiki.API.Types as MW ( PageTitle(..) )
import qualified MediaWiki.API as MW
import qualified MediaWiki.API.Query.Search.Import as MW


data SignalData = SignalGithub Github.Payload | SignalReload

data IrcThreadQuitReason = QuitReasonReload | QuitReasonQuit

githubThread :: Config.Config -> TChan SignalData -> IO ()
githubThread config chan = start 4567 $ M.fromList [("/stuff", (onEvent config chan))]
-- githubThread config chan = start 2082 $ M.fromList [("/stuff", (onEvent config chan))]

onEvent :: Config.Config -> TChan SignalData -> Github.Payload -> IO ()
onEvent config tchan event = do
    case event of
        Github.PullRequest event -> do
            let repo = Github.pullRequestEventRepository event
                action = T.unpack $ Github.pullRequestEventAction event
                sender_user = Github.pullRequestEventSender event
                sender = T.unpack $ Github.userLogin $ sender_user
                pr = Github.pullRequestEventData event
                pr_id = Github.pullRequestNumber pr
                title = T.unpack $ Github.pullRequestTitle pr
                url = B.unpack $ Github.pullRequestHtmlUrl pr

                repo_full_name = T.unpack $ Github.repoFullName repo
                split_repo_full_name = break (=='/') repo_full_name
                repo_owner = fst $ split_repo_full_name
                repo_name = drop 1 $ snd split_repo_full_name

                pr_base = Github.pullRequestBase pr
                pr_head = Github.pullRequestHead pr

                base_sha = B.unpack $ Github.pullRequestCommitSha pr_base

                head_repo_owner = takeWhile (/='/') $ T.unpack $ Github.repoFullName $ Github.pullRequestCommitRepo pr_head
                head_sha = B.unpack $ Github.pullRequestCommitSha pr_head

                message = "Hi! Here's an archive of versions of this PR:" ++ githubNewline ++ githubNewline ++ pr_diff
                githubNewline = "\r\n"

                -- Assuming the base repo is the repo that sent this webhook...
                pr_diff = (B.unpack $ Github.pullRequestUpdatedAt pr) ++ ": https://github.com/" ++ repo_full_name ++ "/compare/" ++ base_sha ++ "..." ++ head_repo_owner ++ ":" ++ head_sha

            when (action == "opened" || action == "synchronize") $ do
                void $ GHAPI.createComment (githubAuth config) repo_owner repo_name pr_id message

        _ -> return ()
    atomically $ writeTChan tchan $ SignalGithub event

githubAuth :: Config.Config -> GHAPI.GithubAuth
githubAuth config = GHAPI.GithubOAuth $ T.unpack $ Config.configAuthToken config

main = controlThread (Config.Config { Config.configNetworks = [], Config.configAuthToken = "", Config.configKPopVideos = [] }) M.empty

type IRCNetwork = (T.Text, Int)
type IRCChan = T.Text
type IRCNetworkConn = M.Map IRCNetwork Handle

ircNetworkFromConfig :: Config.Network -> IRCNetwork
ircNetworkFromConfig network = (Config.networkServer network, Config.networkPort network)

-- Get list of IRC networks (as a servername + port) registered in the given configured networks
ircNetworksInConfig :: [Config.Network] -> [IRCNetwork]
ircNetworksInConfig networks = map ircNetworkFromConfig networks

controlThread :: Config.Config -> IRCNetworkConn -> IO ()
controlThread oldconfig connections = do
    maybeConfig <- Config.readConfig "config.json"
    case maybeConfig of
        Nothing -> putStrLn "Couldn't parse config!" -- TODO: When reloading, reject the new configuration instead
        Just config -> do
                         githubChan <- atomically $ newBroadcastTChan -- TODO: Recreate if auth token changed
                         githubThreadId <- forkIO $ githubThread config githubChan

                         -- Disconnect from servers that were removed from the config
                         list_removed_conns <- forM removed_servers (\server -> do
                                                    putStrLn $ "Disconnecting from .. " ++ show server
                                                    let h = connections M.! server
                                                    disconnectFromIrcNetwork h
                                                    return (server,h))

                         -- Connect to servers that were added to the config (and join all channels)
                         list_new_conns <- forM added_servers (\network -> do
                                                               let ircnetwork = ircNetworkFromConfig network
                                                                   channels = map Config.channelName $ Config.networkChannels network
                                                                   charset = Config.networkCharset network
                                                               putStrLn $ "Connecting to .. " ++ show ircnetwork
                                                               h <- connectToIrcNetwork ircnetwork charset
                                                               forM channels $ joinIrcChan h
                                                               return (ircnetwork,h))

                         -- Collect list of all connections
                         let server_map = M.union (M.filter (`notElem` (M.fromList list_removed_conns)) connections) (M.fromList list_new_conns)

                         -- loop through common networks and connect/disconnect to/from changed channels
                         -- TODO: Zipping here only works when the networks where defined in the same order in the old/new config files.
                         forM_ (zip common_networks_new common_networks_old) (\(network_new,network_old) -> do
                            let server = Config.networkServer network_new
                                port = Config.networkPort network_new
                                h = server_map M.! (server,port)
                                channels_new = map Config.channelName $ Config.networkChannels network_new
                                channels_old = map Config.channelName $ Config.networkChannels network_old
                                channels_added = filter (`notElem` channels_old) channels_new
                                channels_removed = filter (`notElem` channels_new) channels_old
                            forM_ channels_removed $ leaveIrcChan h
                            forM_ channels_added $ joinIrcChan h
                            )

                         -- Enter processing loops (one thread per IRC network)
                         quitReason <- mapConcurrently (\network -> do
                            let server = Config.networkServer network
                                port = Config.networkPort network
                                h = server_map M.! (server,port)
                            controlReadChan <- atomically $ dupTChan githubChan -- TODO: Would prefer cloneTChan instead!
                            ircNetworkThread config network h controlReadChan
                            ) (Config.configNetworks config)

                         killThread githubThreadId -- TODO: shut this down more gracefully!

                         -- For now, assuming all threads quit for the same reason
                         case head quitReason of
                             QuitReasonReload -> do controlThread config server_map
                             _ -> do controlThread config server_map --return ()
                       where
                         servers = ircNetworksInConfig (Config.configNetworks config)
                         old_servers = ircNetworksInConfig (Config.configNetworks oldconfig)
                         added_servers = filter ((`notElem` old_servers). ircNetworkFromConfig) (Config.configNetworks config)
                         removed_servers = filter (`notElem` servers) old_servers :: [IRCNetwork]
                         common_networks_new = filter ((andPred (`elem` servers) (`elem` old_servers)) . ircNetworkFromConfig) (Config.configNetworks config)
                         common_networks_old = filter ((andPred (`elem` servers) (`elem` old_servers)) . ircNetworkFromConfig) (Config.configNetworks oldconfig)
                         andPred pred1 pred2 = \inp -> (pred1 inp) && (pred2 inp)

-- Connect to the given IRC network
connectToIrcNetwork :: IRCNetwork -> Config.TextEnc -> IO Handle
connectToIrcNetwork (server,port) charset = do
    h <- connectTo (T.unpack server) (PortNumber (fromIntegral port))
    hSetEncoding h $ case charset of
            Config.Utf8 -> utf8
            Config.Char8 -> char8
    hSetNewlineMode h $ NewlineMode { inputNL = nativeNewline, outputNL = CRLF }
    hSetBuffering h NoBuffering
    return h

-- Disconnect from the given IRC network
disconnectFromIrcNetwork :: Handle -> IO ()
disconnectFromIrcNetwork h = do
    write h "QUIT" ":Exiting"
    hClose h

joinIrcChan :: Handle -> IRCChan -> IO ()
joinIrcChan h chan = do
    T.putStrLn $ "Joining " `mappend` chan
    write h "JOIN" chan

leaveIrcChan :: Handle -> IRCChan -> IO ()
leaveIrcChan h chan = do
    T.putStrLn $ "Leaving " `mappend` chan
    write h "PART" chan

-- Worker thread handling all communication to a particular IRC network
ircNetworkThread :: Config.Config -> Config.Network -> Handle -> TChan SignalData -> IO (IrcThreadQuitReason)
ircNetworkThread config network h githubChan = do
    let
        nick = Config.networkNick network
        channels = Config.networkChannels network
        watching_channels = filter (watches_some_repo) channels
        watches_some_repo = null . Config.channelWatchedGithubRepos
    write h "NICK" nick
    write h "USER" (nick `mappend` " 0 * :neobot")
    mapM_ (\chan -> write h "JOIN" $ Config.channelName chan) channels
    eventLoop config network githubChan h


write :: Handle -> T.Text -> T.Text -> IO ()
write h s t = do
    T.hPutStrLn h $ T.concat [s, " ", t, "\r\n"]
    T.putStrLn $ T.concat ["> Sending \"", s, " ", t, "\"\n"]

data ListenResult = ListenResultSignalData SignalData | ListenResultIRCLine T.Text | ListenResultConnectionLost

listen :: TChan SignalData -> Handle -> IO (ListenResult)
listen controlChan h = do
    -- Concurrently wait for an external signal or an IRC message to be ready
    ret <- race (atomically $ peekTChan controlChan) (try $ hWaitForInput h (-1))
    case ret of
        -- consume data and process it (repeat if the data is not actually valid)
        Left _ -> do
            signal <- atomically $ readTChan controlChan
            return $ ListenResultSignalData signal
        Right (Right hasInput) ->
            if hasInput
                then do
                    input <- T.hGetLine h -- TODO: Catch exceptions!
                    return $ ListenResultIRCLine input
                else listen controlChan h
        Right (Left (DecodeError _ _)) -> do
            -- Switch encoding to raw bytes and skip the input line, then go back to listening
            (Just old_enc) <- hGetEncoding h -- Returns nothing for binary mode, which we are not using!
            hSetEncoding h char8
            hGetLine h
            hSetEncoding h old_enc
            listen controlChan h
        Right (Left some_exc) ->
            T.putStrLn "Caught exception during hWaitForInput"
            >> (catchIOError (throw some_exc) $ \io_exc ->
                if isEOFError io_exc
                    then (T.putStrLn "Caught EOF exception, rethrowing" >> return ListenResultConnectionLost)
                    else (T.putStrLn "Caught non-EOF IO exception, rethrowing" >> throw some_exc))
            -- This may be an EOF error. We should sleep for a few seconds and then attempt to reconnect to the server here!
            -- For now, we just rethrow the exception
            -- TODO: Reconnect and return new handle instead (connectToIrc)

-- Listen for any kind of event which might be relevant for this IRC session
eventLoop :: Config.Config -> Config.Network -> TChan SignalData -> Handle -> IO (IrcThreadQuitReason)
eventLoop config network controlChan h = do
    ret <- listen controlChan h
    case ret of
        ListenResultSignalData signal ->
            case signal of
                SignalGithub githubPayload -> handleGithubPayload config network githubPayload h >> repeat h
                SignalReload -> return (QuitReasonReload)

        ListenResultIRCLine input -> do
            -- TODO: Clean this up!
            let stuff "!reload" = atomically $ writeTChan controlChan SignalReload

            case T.words input of
                _ : x : chan_name : tail | (x=="PRIVMSG") && ((T.drop 1 $ T.unwords tail) == "!reload") -> stuff $ T.drop 1 $ T.unwords tail
                _ -> T.putStrLn $ "Unknown command " `mappend` input

            handleMessage config network h input >> repeat h
        ListenResultConnectionLost -> do
                                        threadDelay 5000000 -- Sleep for 5 seconds until attempting to reconnect
                                        newConnection <- connectToIrcNetwork (ircNetworkFromConfig network) $ Config.networkCharset network -- repeat -- TODO: Reconnect and change handle!
                                        repeat newConnection
    where
        repeat connectionHandle = eventLoop config network controlChan connectionHandle

-- Handle an incoming Github webhook for the current IRC network
handleGithubPayload :: Config.Config -> Config.Network -> Github.Payload -> Handle -> IO ()
handleGithubPayload config network payload h =
    case payload of
        Github.Push         event -> sequence_ $ map (\commit -> notify_commit network event commit h) (Github.pushEventCommits event)
        Github.IssueComment event -> notify_issue_comment network event h
        Github.Issue        event -> notify_issue config network event h
        Github.PullRequest  event -> notifyPullRequest config network event h
        _                         -> print "Received payload, but not sure what to do with it!"

-- TODO: This should take a ByteString instead!
decodeMessage :: T.Text -> IRC.Event T.Text
decodeMessage (T.stripPrefix "PING :" -> Just message) = IRC.Event "RAW" (IRC.Server "SERVERNAME") (IRC.Ping message Nothing) -- NOTE: We ignore the second parameter, for now...
decodeMessage (T.stripPrefix (T.singleton ':') -> Just str) = let command = T.words str in
    case command of
        user : x : chan_name : tail | (x=="PRIVMSG") -> IRC.Event "" (IRC.Channel "CHAN_NAME" sender_name) (IRC.Privmsg chan_name (Right $ T.drop 1 $ T.unwords tail))
                                                where
                                                 sender_name = T.takeWhile (/='!') user
        otherwise -> IRC.Event "" (IRC.User "Unknown") (IRC.RawMsg str)

decodeMessage x = IRC.Event "" (IRC.User "Unknown") (IRC.RawMsg x)

-- Handle an incoming IRC message for the current IRC network
handleMessage :: Config.Config -> Config.Network -> Handle -> T.Text -> IO ()
handleMessage config network h t = --do
    let s = T.init t in
        case decodeMessage s of
            IRC.Event _ _ (IRC.Ping message Nothing) -> write h "PONG" (":" `mappend` message)
            IRC.Event _ (IRC.Channel _ sender_name) (IRC.Privmsg chan_name (Right message)) -> do
                print $ "Reading message.."
                case lookup_channel_in_network network chan_name of
                    Nothing -> return ()
                    Just chan -> case message of
                        -- K-pop handler
                        "!kpop" -> sendRandomKPopVideo h chan (Config.configKPopVideos config)
                        (T.stripPrefix "!kpop " -> Just filter_text) -> sendRandomKPopVideo h chan $ filter (videoMatchesFilter $ T.toLower filter_text) $ Config.configKPopVideos config
                        -- !say with and without target channel
                        (T.stripPrefix "!say " -> Just x) | (T.isPrefixOf "#" x) -> case maybeChan of
                            Nothing -> privmsg h chan $ T.concat ["I don't know the channel ", target_chan_name, "!"]
                            Just target_chan -> privmsg h target_chan message
                            where target_chan_name = head $ T.words x
                                  maybeChan = lookup_channel_in_network network target_chan_name
                                  message = T.unwords $ tail $ T.words x
                        (T.stripPrefix "!say " -> Just x) -> privmsg h chan x
                        (T.stripPrefix "!issue " -> Just x) -> getissue config network chan h x -- TODO: Change getissue to return a printable value instead of making it write stuff
                        -- Channel-local handler
                        otherwise -> do
                            in_tchan <- atomically $ newTChan -- dummy, not used here for now.
                            out_tchan <- atomically $ newTChan
                            handleIRCChannelMessage (IRCChannelContext chan in_tchan out_tchan) (IRCMessage sender_name message)
                            out_message <- atomically $ tryReadTChan out_tchan
                            case out_message of
                                Just (chan_name, message) -> sendToChannel h chan_name message
                                Nothing -> return ()
            otherwise -> T.putStrLn $ T.concat ["Received unknown message ", s, "\n"]

lookup_channel_in_network :: Config.Network -> T.Text -> Maybe Config.Channel
lookup_channel_in_network network channel = find ((channel==) . Config.channelName) $ Config.networkChannels network

lookup_channel_in_config :: Config.Config -> T.Text -> T.Text -> Maybe Config.Channel
lookup_channel_in_config config network channel =
    maybe Nothing channel_match network_match
    where network_match = lookup_network_in_config config network
          channel_match network = lookup_channel_in_network network channel

lookup_network_in_config :: Config.Config -> T.Text -> Maybe Config.Network
lookup_network_in_config config network = find ((network==) . Config.networkName) $ Config.configNetworks config

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
notifyPullRequest :: Config.Config -> Config.Network -> Github.PullRequestEvent -> Handle -> IO ()
notifyPullRequest config network event h =
    let
        repo = Github.pullRequestEventRepository event
        repo_full_name = T.unpack $ Github.repoFullName repo
        split_repo_full_name = break (=='/') repo_full_name
        repo_owner = fst $ split_repo_full_name
        repo_name = drop 1 $ snd split_repo_full_name

        filtered_channels = getChannelsObservingRepo network repo

        send_notification chan = privmsg h chan $ T.concat ["\x03\&12\x02", sender, "\x0F ", action, " pull request #", pr_id_string, ": \"", title, "\", ", url]

        action = Github.pullRequestEventAction event
        sender_user = Github.pullRequestEventSender event
        sender = Github.userLogin $ sender_user
        pr = Github.pullRequestEventData event
        pr_id = Github.pullRequestNumber pr
        pr_id_string = T.show pr_id
        title = Github.pullRequestTitle pr
        url = T.pack $ B.unpack $ Github.pullRequestHtmlUrl pr
    in do
        forM_ filtered_channels send_notification
        when (pr_id == 1999 && action == "opened") $ do
            let issue = GHAPI.NewIssue title (Just $ T.unpack comment) Nothing Nothing (Just [])
                title = "Hi, this is neobot, and for #2000 we need to throw the PARTY OF THE YEAR"
                comment = mappend "Sorry guys, just had to take this one too.\n\nluv ya ♥\n\n\nPS: Have some K-pop and other fancy music!\n" kpop_videos
                kpop_videos = T.unlines $ map makeVideoString $ Config.configKPopVideos config
            void $ createIssue (githubAuth config) repo_owner repo_name issue

data IssueOrPullRequest = IOPRIssue | IOPRPullRequest deriving Eq

-- Insert U+FEFF as a dummy character that is usually not displayed by IRC clients but which prevents unnecessary highlighting
-- The dummy character is inserted after the first letter in the nickname
nonpingingName :: T.Text -> T.Text
nonpingingName name = T.intercalate (T.singleton '\xfeff') $ map ($ name) [T.take 1, T.drop 1]

buildIssueActionString :: T.Text -> T.Text -> T.Text -> IssueOrPullRequest -> T.Text -> T.Text -> T.Text -> T.Text
buildIssueActionString action_author comment_author action issue_or_pr ticket_id body url =
    T.concat [
        "\x03\&12\x02", nonpingingName action_author, "\x0F ",
        case action of
            "opened" -> "opened"
            "closed" -> "closed"
            "labeled" -> "labeled"
            "created" -> "commented on"
            "synchronize" -> "synchronized"
            _ | action_author == comment_author -> T.concat [action, " their comment on"]
              | otherwise                       -> T.concat [action, " ", nonpingingName comment_author, "'s comment on"],
        " ", if issue_or_pr == IOPRIssue then "issue" else "pull request", " #", ticket_id, ": \"", body, "\", ", url]

-- Notify the IssueEvent to all interested channels on the given network
notify_issue :: Config.Config -> Config.Network -> Github.IssueEvent -> Handle -> IO ()
notify_issue config network event h =
    let
        -- send_notification chan = privmsg h chan $ T.concat ["\x03\&12\x02", sender, "\x0F ", action, " issue #", issue_id_string, ": \"", title, "\", ", url]
        send_notification chan = privmsg h chan $ buildIssueActionString sender sender action IOPRIssue issue_id_string title url
        repo = Github.issueEventRepository event
        repo_full_name = T.unpack $ Github.repoFullName repo
        split_repo_full_name = break (=='/') repo_full_name
        repo_owner = fst $ split_repo_full_name
        repo_name = drop 1 $ snd split_repo_full_name
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueEventIssue event
        title = Github.issueTitle issue
        issue_id = Github.issueNumber issue
        issue_id_string = T.show $ issue_id
        -- I think "action_author" and "issue_author" should be equivalent here!
        sender_user = Github.issueEventSender event
        sender = Github.userLogin $ sender_user
        url = T.pack $ B.unpack $ Github.issueHtmlUrl issue
        action = Github.issueEventAction event
    in do
        forM_ filtered_channels send_notification
        when (issue_id == 1999 && action == "opened") $ do
            let issue = GHAPI.NewIssue title (Just $ T.unpack comment) Nothing Nothing (Just [])
                title = "Hi, this is neobot, and for #2000 we need to throw the PARTY OF THE YEAR"
                comment = mappend "Sorry guys, just had to take this one too.\n\nluv ya ♥\n\n\nPS: Have some K-pop and other fancy music!\n" kpop_videos
                kpop_videos = T.unlines $ map makeVideoString $ Config.configKPopVideos config
            void $ createIssue (githubAuth config) repo_owner repo_name issue

-- Notify the IssueCommentEvent to all interested channels on the given network
notify_issue_comment :: Config.Network -> Github.IssueCommentEvent -> Handle -> IO ()
notify_issue_comment network event h = unless (T.isInfixOf "this is neobot" body) $ forM_ filtered_channels send_notification
    where
        -- send_notification chan = privmsg h chan $ T.concat ["\x03\&12\x02", author, "\x0F ", action, " ", issue_or_pr_text, " #", issue_id, ": \"", body, "\", ", url]
        send_notification chan = privmsg h chan $ buildIssueActionString action_author comment_author action issue_or_pr issue_id body url
        --action = case Github.issueCommentEventAction event of
        --            "created" -> "commented on"
        --            other -> T.append other " their comment on"
        action = Github.issueCommentEventAction event
        repo = Github.issueCommentEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueCommentEventIssue event
        issue_id = T.show $ Github.issueNumber issue
        comment = Github.issueCommentEventComment event
        action_author = Github.userLogin $ Github.issueCommentEventSender event
        comment_author = Github.userLogin $ Github.commentUser comment
        body = shorten_to_length 100 $ firstLineOnly $ Github.commentBody comment
        url = T.pack $ B.unpack $ Github.commentHtmlUrl comment
        -- issue_or_pr_text = if (isUrlToPullRequest url) then "pull request" else "issue"
        issue_or_pr = if (isUrlToPullRequest url) then IOPRPullRequest else IOPRIssue
        shorten_to_length n str = if T.length str <= n then str else (T.take (n - 6) str) `mappend` " [...]"

isUrlToPullRequest :: T.Text -> Bool
isUrlToPullRequest url = T.isInfixOf "/pull/" url

firstLineOnly :: T.Text -> T.Text
firstLineOnly str = T.takeWhile (/= '\r') $ T.takeWhile (/= '\n') $ str -- ugly way to make sure both \r and \n are recognized as newline characters; TODO: Might want to try universalNewlineMode!

-- Notify the PushEvent's commit to all interested channels on the given network
notify_commit :: Config.Network -> Github.PushEvent -> Github.Commit -> Handle -> IO ()
notify_commit network event commit h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ T.concat ["[", format_boldpink, repo_name, format_reset, "] new patch by ", author, ": ",
                                                                 format_italics, message, format_reset, " ", format_darkblue, url]
        format_boldpink = "\x03\&13\x02"
        format_darkblue = "\x03\&02"
        format_purple   = "\x03\&06"
        format_italics  = "" -- "\x1D" -- doesn't seem to be supported in irssi :(
        format_reset    = "\x0F"
        repo = Github.pushEventRepository event
        repo_name = Github.repoName repo
        filtered_channels = getChannelsObservingRepo network repo
        author = nonpingingName $ Github.simpleUserName $ Github.commitCommitter commit
        message = firstLineOnly $ Github.commitMessage commit
        url = T.show $ B.unpack $ Github.commitUrl commit

data IRCMessage = IRCMessage
    { messageSender :: T.Text
    , messageContent :: T.Text
    }

data IRCChannelEvent = IRCChannelEventMessage IRCMessage

data IRCChannelContext = IRCChannelContext
    { channelContextConfig :: Config.Channel
    , channelContextInStream :: TChan IRCChannelEvent
    , channelContextOutStream :: TChan (T.Text, T.Text) -- Channel name and message
    }

sendMessage :: IRCChannelContext -> T.Text -> IO ()
sendMessage context message = atomically $ writeTChan (channelContextOutStream context) (Config.channelName $ channelContextConfig context, message)

ircChannelThread :: IRCChannelContext -> IO ()
ircChannelThread context = do
    event <- atomically $ readTChan $ channelContextInStream context
    case event of
        IRCChannelEventMessage message -> handleIRCChannelMessage context message
        -- TODO: On join events, keep a timestamp for the joining user
    ircChannelThread context

handleIRCChannelMessage :: IRCChannelContext -> IRCMessage -> IO ()
-- regular commands
handleIRCChannelMessage context (IRCMessage "neobrain"  "!quit") = do
    sendMessage context "Alright, I'm on it!"
    -- write h "QUIT" ":Exiting" -- TODO: Re-enable!
    exitWith ExitSuccess
handleIRCChannelMessage context (IRCMessage _  "!quit")   = sendMessage context "Yeah... no. Shouldn't you be working instead of trying to mess with me?"
handleIRCChannelMessage context (IRCMessage _ "!help")    = sendMessage context "Supported commands: !3dbrew, !about, !gpl, !help, !issue N, !kpop, !love, !say, !xkcd"
handleIRCChannelMessage context (IRCMessage _ "!about")   = sendMessage context "I'm indeed really awesome! Learn more about me in #neobot."
handleIRCChannelMessage context (IRCMessage _ "!love")    = sendMessage context "Haskell is love. Haskell is life."
handleIRCChannelMessage context (IRCMessage _ "!gpl")     = sendMessage context "RELEASE THE SOURCE ALREADY!!!1"
handleIRCChannelMessage context (IRCMessage _ x) | ("gpl gpl gpl" `T.isInfixOf` (T.toLower x) && (Config.channelReplyToCatchPhrases (channelContextConfig context))) = sendMessage context "RELEASE THE SOURCE ALREADY!!!1"
handleIRCChannelMessage context (IRCMessage _ x) | "!xkcd " `T.isPrefixOf` x = sendMessage context $ "https://xkcd.com/" `T.append` (T.drop 6 x) -- TODO: Use https://xkcd.com/json.html to print the title!
handleIRCChannelMessage context (IRCMessage _ "!( ͡° ͜ʖ ͡°)") = sendMessage context "lenny"
handleIRCChannelMessage context (IRCMessage _ "!lenny")    = sendMessage context "( ͡° ͜ʖ ͡°)"

handleIRCChannelMessage context (IRCMessage _ (T.stripPrefix "!3dbrew " -> Just search_term)) = do
    mb <- MW.webGetXml MW.stringXml "http://www.3dbrew.org/w/" req -- TODO: Make the wiki url configurable
    case mb of
        Just response | not $ null $ MW.srPages response -> do
            let matches = MW.srPages response
                num_matches = length matches
                match_links = map (\x -> T.pack . ("https://www.3dbrew.org/wiki/" ++) . (intercalate "_") . splitOn " " . MW.pgTitle $ x) matches
                reply_prefix = (T.pack $ (if num_matches == max_results then "First " else "")) `T.append` (T.show num_matches) `T.append` (T.pack " search results on 3dbrew: ")
                reply = reply_prefix `T.append` (T.intercalate ", " match_links)
            sendMessage context reply
        _ -> sendMessage context "No matching 3dbrew pages found!"
    where max_results = 3
          search_request = MW.SearchRequest { MW.srSearch = T.unpack search_term, MW.srNamespaces = [], MW.srWhat = False, MW.srRedirects = True, MW.srOffset = Nothing, MW.srLimit = Just (max_results) }
          req = MW.emptyXmlRequest (MW.mkQueryAction (MW.queryPage "") search_request)

-- keyword recognition
handleIRCChannelMessage context (IRCMessage _ x) | isGreeting x && (Config.channelReplyToGreetings $ channelContextConfig context) = sendMessage context $ T.concat ["Hi! Welcome to ", Config.channelName $ channelContextConfig context, ". Do you have a question on your mind? I'm just a bot, but lots of real people are hanging around here and may be able to help. Make sure to stick around for more than a few minutes to give them a chance to reply, though!"]
handleIRCChannelMessage context (IRCMessage _ x) | ("msvcp" `T.isInfixOf` (T.toLower x)) = sendMessage context $ "Got an MSVCP dll error? Download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 ."
handleIRCChannelMessage context (IRCMessage _ x) | ("0x0000007b" `T.isInfixOf` (T.toLower x)) = sendMessage context $ "Got a 0x0000007b error when starting Citra? Don't download random .dll files from the internet! Undo your local modifications, and download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 instead."
-- TODO: Recognize /pull/ as part of a github url

handleIRCChannelMessage context (IRCMessage _ x) | ("emucr" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "Please don't support emucr by downloading our (or any, really) software from emucr."
handleIRCChannelMessage context (IRCMessage _ x) | ("pokemon" `T.isInfixOf` (T.toLower x)) && ("?" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "Pokémon SUCKS!"
handleIRCChannelMessage context (IRCMessage _ x) | ("pokemons" `T.isInfixOf` (T.toLower x)) && ("?" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "\"pokemons\", seriously?"
handleIRCChannelMessage context (IRCMessage _ x) | (" rom " `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) && (" download" `T.isInfixOf` (T.toLower x)) = sendMessage context $ "We don't support piracy here."
handleIRCChannelMessage context (IRCMessage _ x) | (" support " `T.isInfixOf` (T.toLower x)) && (" android " `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "Android support is far from being a priority, currently."
handleIRCChannelMessage context (IRCMessage _ x) | ("3dmoo" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "3dmoo, hah. Good one!"
handleIRCChannelMessage context (IRCMessage _ x) | ("tronds" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases $ channelContextConfig context) = sendMessage context $ "TronDS is a very successful 3DS emulator, indeed. Happy Opposite Day!"

handleIRCChannelMessage context (IRCMessage _ x) = T.putStrLn $ T.concat ["> ", T.concat ["Ignoring message \"", x, "\" from channel ", Config.channelName $ channelContextConfig context, "\n"]]



isGreeting :: T.Text -> Bool
isGreeting s = (T.unpack $ T.toLower s) =~ ("^(hello|hey|hi|helo|helllo|anyone?|anyone here|anybody here|nobody|help|h+e+l+o+)" :: String)

myreadMaybe :: Read a => T.Text -> Maybe a
myreadMaybe s = case reads (T.unpack s) of
                  [(val, "")] -> Just val
                  _           -> Nothing

getissue :: Config.Config -> Config.Network -> Config.Channel -> Handle -> T.Text -> IO ()
getissue config network channel h x =
    case maybeIssueNum of
        Nothing -> privmsg h channel $ "That is not a valid issue number."
        Just issueNum ->
            do
                possibleIssue <- GHAPI.issue' auth repo_owner repo_name issueNum
                case possibleIssue of
                    (Left err) -> privmsg h channel $ "Error: " `mappend` (T.show err)
                    (Right issue) -> let url = maybe "" T.pack $ GHAPI.issueHtmlUrl issue
                                         issue_or_pr_text = if (isUrlToPullRequest url) then "Pull request " else "Issue "
                                         reported_or_submitted_text = if (isUrlToPullRequest url) then ", submitted by " else ", reported by "
                                     in privmsg h channel $ T.concat [issue_or_pr_text, T.show issueNum, reported_or_submitted_text, T.pack $ GHAPI.githubOwnerLogin (GHAPI.issueUser issue), ": \"", T.pack $ GHAPI.issueTitle issue, "\", ", url]

    where maybeIssueNum = myreadMaybe x :: Maybe Int
          repo_owner = T.unpack $ Config.watchedRepoOwner $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          repo_name = T.unpack $ Config.watchedRepoName $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          auth = Just $ GHAPI.GithubOAuth $ T.unpack $ Config.configAuthToken config

-- TODO: Change to "Channel -> Handle -> String -> IO ()" for consistency!
privmsg :: Handle -> Config.Channel -> T.Text -> IO ()
privmsg h chan s = sendToChannel h chan_name s
    where chan_name = Config.channelName chan

sendToChannel :: Handle -> T.Text -> T.Text -> IO ()
sendToChannel h chan s = write h "PRIVMSG" $ T.concat [chan, " :", s]

rollIntInRange :: Int -> Int -> IO Int
rollIntInRange min max = R.getStdRandom (R.randomR (min, max))

getRandomElement :: [a] -> IO a
getRandomElement list = do
    index <- rollIntInRange 0 (length $ tail list)
    return $ list !! index

makeVideoString :: Config.KPopVideo -> T.Text
makeVideoString video = T.concat [artist, " - \"", title, "\": ", url]
    where
        artist = Config.kpopvideoArtist video
        title = Config.kpopvideoTitle video
        url = Config.kpopvideoUrl video

-- TODO: This currently also searched the URL for the given text, which is not what we want!
videoMatchesFilter :: T.Text -> Config.KPopVideo -> Bool
videoMatchesFilter filter_text video = T.isInfixOf filter_text $ T.toLower $ makeVideoString video

-- Send a randomly selected video from the given list to the given channel
sendKPopVideo :: Handle -> Config.Channel -> Config.KPopVideo -> IO ()
sendKPopVideo h chan video = do
    T.putStrLn $ makeVideoString video
    privmsg h chan $ makeVideoString video

sendRandomKPopVideo :: Handle -> Config.Channel -> [Config.KPopVideo] -> IO ()
sendRandomKPopVideo h chan [] = privmsg h chan "No videos found!"
sendRandomKPopVideo h chan videos = do
    video <- getRandomElement videos
    sendKPopVideo h chan video
