{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-} -- We need this to pattern match Text, currently..

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
import Github.Issues.Comments as GHAPI

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Char as Char

import qualified BasicPrelude as T (show)

import Data.Maybe (isJust, fromJust)

import Control.Monad (forM_, forM, void, when)
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently, wait, async, race)
import Control.Concurrent.STM.TChan

import Ghhooks

import qualified Config

import qualified Github.PostReceive as Github
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

import qualified System.Random as R

import Network.HTTP.Conduit

import Text.Regex.Posix

data SignalData = SignalGithub Github.Payload | SignalReload

data IrcThreadQuitReason = QuitReasonReload | QuitReasonQuit

githubThread :: Config.Config -> TChan SignalData -> IO ()
githubThread config chan = start 4567 $ M.fromList [("/stuff", (onEvent config chan))]

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

                message = "Hi, this is neobot, using neobrain's account. I'm keeping an archive of versions of this PR:" ++ githubNewline ++ githubNewline ++ pr_diff
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
                                                               putStrLn $ "Connecting to .. " ++ show ircnetwork
                                                               h <- connectToIrcNetwork ircnetwork
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
connectToIrcNetwork :: IRCNetwork -> IO Handle
connectToIrcNetwork (server,port) = do
    h <- connectTo (T.unpack server) (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetEncoding h utf8
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

listen :: TChan SignalData -> Handle -> IO (Either SignalData T.Text)
listen controlChan h = do
    -- Concurrently wait for an external signal or an IRC message to be ready
    ret <- race (atomically $ peekTChan controlChan) (hWaitForInput h (-1))
    case ret of
        -- consume data and process it (repeat if the data is not actually valid)
        Left _ -> do
            signal <- atomically $ readTChan controlChan
            return $ Left signal
        Right hasInput ->
            if hasInput
                then do
                    input <- T.hGetLine h
                    return $ Right input
                else listen controlChan h

-- Listen for any kind of event which might be relevant for this IRC session
eventLoop :: Config.Config -> Config.Network -> TChan SignalData -> Handle -> IO (IrcThreadQuitReason)
eventLoop config network controlChan h = do
    ret <- listen controlChan h
    case ret of
        Left signal ->
            case signal of
                SignalGithub githubPayload -> handleGithubPayload network githubPayload h >> repeat
                SignalReload -> return (QuitReasonReload)

        Right input -> do
            -- TODO: Clean this up!
            let stuff "!reload" = atomically $ writeTChan controlChan SignalReload

            case T.words input of
                _ : x : chan_name : tail | (x=="PRIVMSG") && ((T.drop 1 $ T.unwords tail) == "!reload") -> stuff $ T.drop 1 $ T.unwords tail
                _ -> T.putStrLn $ "Unknown command " `mappend` input

            handleMessage config network h input >> repeat
    where
        repeat = eventLoop config network controlChan h

--TODO: Formalize IRC messages in some "data IRCMessage = ..."

-- Handle an incoming Github webhook for the current IRC network
handleGithubPayload :: Config.Network -> Github.Payload -> Handle -> IO ()
handleGithubPayload network payload h =
    case payload of
        Github.Push         event -> sequence_ $ map (\commit -> notify_commit network event commit h) (Github.pushEventCommits event)
        Github.IssueComment event -> notify_issue_comment network event h
        Github.Issue        event -> notify_issue network event h
        Github.PullRequest  event -> notifyPullRequest network event h
        _                         -> print "Received payload, but not sure what to do with it!"

-- Handle an incoming IRC message for the current IRC network
handleMessage :: Config.Config -> Config.Network -> Handle -> T.Text -> IO ()
handleMessage config network h t = do
    if ping s
    then pong s
    else do
        print $ "Reading message.."
        evalraw config network h s
    T.putStrLn s
    where
        s = T.init t
        forever a = do a; forever a

        ping x    = "PING :" `T.isPrefixOf` x
        pong x    = write h "PONG" (":" `mappend` T.drop 6 x)

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
notifyPullRequest :: Config.Network -> Github.PullRequestEvent -> Handle -> IO ()
notifyPullRequest network event h = forM_ filtered_channels send_notification
    where
        repo = Github.pullRequestEventRepository event
        filtered_channels = getChannelsObservingRepo network repo

        send_notification chan = privmsg h chan $ T.concat [sender, " ", action, " pull request #", pr_id, ": \"", title, "\" --- ", url]

        action = Github.pullRequestEventAction event
        sender_user = Github.pullRequestEventSender event
        sender = Github.userLogin $ sender_user
        pr = Github.pullRequestEventData event
        pr_id = T.show $ Github.pullRequestNumber pr
        title = Github.pullRequestTitle pr
        url = T.show $ B.unpack $ Github.pullRequestHtmlUrl pr

-- Notify the IssueEvent to all interested channels on the given network
notify_issue :: Config.Network -> Github.IssueEvent -> Handle -> IO ()
notify_issue network event h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ T.concat [sender, " ", action, " issue #", issue_id, ": \"", title, "\" --- ", url]
        repo = Github.issueEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueEventIssue event
        title = Github.issueTitle issue
        issue_id = T.show $ Github.issueNumber issue
        sender_user = Github.issueEventSender event
        sender = Github.userLogin $ sender_user
        url = T.show $ B.unpack $ Github.issueHtmlUrl issue
        action = Github.issueEventAction event

-- Notify the IssueCommentEvent to all interested channels on the given network
notify_issue_comment :: Config.Network -> Github.IssueCommentEvent -> Handle -> IO ()
notify_issue_comment network event h = forM_ filtered_channels send_notification
    where
        send_notification chan = privmsg h chan $ T.concat ["\x03\&12\x02", author, "\x0F commented on issue #", issue_id, ": \"", body, "\" --- ", url]
        repo = Github.issueCommentEventRepository event
        filtered_channels = getChannelsObservingRepo network repo
        issue = Github.issueCommentEventIssue event
        issue_id = T.show $ Github.issueNumber issue
        comment = Github.issueCommentEventComment event
        author_user = Github.commentUser comment
        author = Github.userLogin $ Github.commentUser comment
        body = shorten_to_length 100 $ firstLineOnly $ Github.commentBody comment
        url = T.show $ B.unpack $ Github.commentHtmlUrl comment
        shorten_to_length n str = if T.length str <= n then str else (T.take (n - 6) str) `mappend` " [...]"

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
        author = Github.simpleUserName $ Github.commitCommitter commit
        message = firstLineOnly $ Github.commitMessage commit
        url = T.show $ B.unpack $ Github.commitUrl commit

-- Handle -> rawmessage
evalraw :: Config.Config -> Config.Network -> Handle -> T.Text -> IO ()
evalraw  config network h (T.stripPrefix (T.singleton ':') -> Just s) = case command of
    user : x : chan_name : tail | (x=="PRIVMSG") -> maybe (return ()) handleMessage (lookup_channel_in_network network chan_name)
                                      | otherwise -> T.putStrLn $ T.concat ["Unknown command ", x, "\n"]
                                           where
                                             handleMessage chan = evalPrivMsg config network chan h sender_name $ T.drop 1 $ T.unwords tail
                                             sender_name = T.takeWhile (/='!') user
    otherwise -> printf $ "INVALID COMMAND \n" ++ (show command)
    where command = T.words s
evalraw _ _ _ s = T.putStrLn $ T.concat ["Received unknown message ", s, "\n"]

-- Config -> Network -> Channel -> Handle -> sender -> message
evalPrivMsg :: Config.Config -> Config.Network -> Config.Channel -> Handle -> T.Text -> T.Text -> IO ()
-- regular commands
--evalPrivMsg _ _ chan h _  "!quit"    = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
evalPrivMsg _ _ chan h _  "!quit"    = privmsg h chan "Yeah... no. Shouldn't you be working instead of trying to mess with me?"
--evalPrivMsg _ _ chan h _ x | "!quit " `isPrefixOf` x   = write h ("QUIT" ": Exiting (" ++ (drop 6 x) ++ ")") >> exitWith ExitSuccess
evalPrivMsg _ _ chan h _ "!help"    = privmsg h chan "Supported commands: !about, !gpl, !help, !issue N, !kpop, !love, !say, !xkcd"
evalPrivMsg _ _ chan h _ "!about"   = privmsg h chan "I'm indeed really awesome! Learn more about me in #neobot."
evalPrivMsg _ _ chan h _ "!love"    = privmsg h chan "Haskell is love. Haskell is life."
evalPrivMsg _ _ chan h _ "!gpl"     = privmsg h chan "RELEASE THE SOURCE ALREADY!!!1"
evalPrivMsg _ _ chan h _ x | ("gpl gpl gpl" `T.isInfixOf` (T.toLower x) && (Config.channelReplyToCatchPhrases chan)) = privmsg h chan "RELEASE THE SOURCE ALREADY!!!1"
evalPrivMsg _ _ chan h _ x | "!xkcd " `T.isPrefixOf` x = privmsg h chan $ "https://xkcd.com/" `T.append` (T.drop 6 x) -- TODO: Use https://xkcd.com/json.html to print the title!

-- K-pop handler
evalPrivMsg config _ chan h _ "!kpop" = sendRandomKPopVideo h chan (Config.configKPopVideos config)
evalPrivMsg config _ chan h _ (T.stripPrefix "!kpop " -> Just filter_text) =
    sendRandomKPopVideo h chan $ filter (videoMatchesFilter $ T.toLower filter_text) $ Config.configKPopVideos config

-- !say with and without target channel
evalPrivMsg _ network source_chan h _ x | "!say #" `T.isPrefixOf` x = case maybeChan of
    Nothing -> privmsg h source_chan $ T.concat ["I don't know the channel ", chan_name, "!"]
    Just chan -> privmsg h chan message
    where chan_name = head $ tail $ T.words x
          maybeChan = lookup_channel_in_network network chan_name
          message = T.unwords $ tail $ tail $ T.words x
evalPrivMsg _ _ chan h _ (T.stripPrefix "!say " -> Just x) = privmsg h chan x

evalPrivMsg config network chan h _ (T.stripPrefix "!issue " -> Just x) = getissue config network chan h x -- TODO: Change getissue to return a printable value instead of making it write stuff
-- keyword recognition
evalPrivMsg _ _ chan h _ x | isGreeting x && (Config.channelReplyToGreetings chan) = privmsg h chan $ T.concat ["Hi! Welcome to ", Config.channelName chan, ". Do you have a question on your mind? I'm just a bot, but lots of real people are hanging around here and may be able to help. Make sure to stick around for more than a few minutes to give them a chance to reply, though!"]
evalPrivMsg _ _ chan h _ x | ("msvcp" `T.isInfixOf` (T.toLower x)) = privmsg h chan $ "Got an MSVCP dll error? Download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 ."
evalPrivMsg _ _ chan h _ x | ("0x0000007b" `T.isInfixOf` (T.toLower x)) = privmsg h chan $ "Got a 0x0000007b error when starting Citra? Don't download random .dll files from the internet! Undo your local modifications, and download and install vc_redist.x64.exe from https://www.microsoft.com/en-us/download/details.aspx?id=48145 instead."
-- TODO: Recognize /pull/ as part of a github url
-- possibly offensive stuff
evalPrivMsg _ _ chan h sender x | ("youtube.com/watch" `T.isInfixOf` (T.toLower x)) = (flip catch) ((\_ -> return ()) :: SomeException -> IO ()) $ do
    contents <- Network.HTTP.Conduit.simpleHttp url
    when (("http://www.youtube.com/user/pcmaker2") `isInfixOf` (wordToChar $ L.unpack contents)) $ do
        write h "KICK" $ T.concat [chan_name, " ", sender, " :Nope, no advertisement of YouTube channels that regularly post prerelease stuff here."]
    where
        url = T.unpack $ head $ filter (("youtube.com/watch" `T.isInfixOf`). T.toLower) $ T.words x
        wordToChar = map (\c -> (toEnum (fromEnum c) :: Char))
        chan_name = Config.channelName chan

evalPrivMsg _ _ chan h _ x | ("emucr" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "Please don't support emucr by downloading our (or any, really) software from emucr."
evalPrivMsg _ _ chan h _ x | ("pokemon" `T.isInfixOf` (T.toLower x)) && ("?" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "No, we won't help you emulating Pokémon."
evalPrivMsg _ _ chan h _ x | (" rom " `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) && (" download" `T.isInfixOf` (T.toLower x)) = privmsg h chan $ "We don't support piracy here."
evalPrivMsg _ _ chan h _ x | (" support " `T.isInfixOf` (T.toLower x)) && (" android " `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "Android support is far from being a priority, currently."
evalPrivMsg _ _ chan h _ x | ("3dmoo" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "3dmoo, hah. Good one!"
evalPrivMsg _ _ chan h _ x | ("tronds" `T.isInfixOf` (T.toLower x)) && (Config.channelReplyToCatchPhrases chan) = privmsg h chan $ "TronDS is a very successful 3DS emulator, indeed. Happy Opposite Day!"
evalPrivMsg _ _ chan h _ x = T.putStrLn $ T.concat ["> ", T.concat ["Ignoring message \"", x, "\" from channel ", Config.channelName chan, "\n"]]

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
                    (Right issue) -> privmsg h channel $ T.concat ["Issue ", T.show issueNum, ", reported by ", T.pack $ GHAPI.githubOwnerLogin (GHAPI.issueUser issue), ": ", T.pack $ GHAPI.issueTitle issue]

    where maybeIssueNum = myreadMaybe x :: Maybe Int
          repo_owner = T.unpack $ Config.watchedRepoOwner $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          repo_name = T.unpack $ Config.watchedRepoName $ head $ Config.channelWatchedGithubRepos channel -- TODO: This is unsafe!
          auth = Just $ GHAPI.GithubOAuth $ T.unpack $ Config.configAuthToken config

-- TODO: Change to "Channel -> Handle -> String -> IO ()" for consistency!
privmsg :: Handle -> Config.Channel -> T.Text -> IO ()
privmsg h chan s = write h "PRIVMSG" $ T.concat [chan_name, " :", s]
    where chan_name = Config.channelName chan

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
