{-# LANGUAGE ScopedTypeVariables #-}

module Irc (
    module Irc,
    IrcServer(..)
) where

import Prelude hiding (log, readFile)

import IrcServer (IrcServer(..), IrcConnection(..), connect, listen)
import IrcMessage
import qualified Logging as Log

import qualified Data.Text as T (Text(..), pack, unpack, isPrefixOf, singleton, drop,
                                break)
import Data.List (sort, sortOn)
import Data.Char (toLower, chr)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

import System.Random (StdGen, getStdGen, randomR, randomRIO)
import System.IO.Strict (readFile)

import Control.Exception (IOException, catch)

import Control.Concurrent (forkIO, forkFinally, ThreadId, threadDelay)
import Control.Concurrent.Chan (writeChan, readChan, getChanContents)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)

import Control.Monad (ap, filterM)
import Control.Monad.State (StateT(..), evalStateT, execStateT, get, put, gets, modify)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask, asks, reader, liftIO)
import Control.Monad.Writer.Lazy (WriterT(..), execWriterT, tell)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Trans (lift)

import Control.Conditional (if', ifM)

data Data = Data { registered :: Bool, hunting :: Bool, duckState :: MVar Hit, duckData :: DuckData, duckTime :: POSIXTime }

data DuckData = DuckData { bangs :: [Hit], friends :: [Hit] }

data Hit = Bang { banger :: Text, bangtime :: Int }
         | Friend { friender :: Text, friendtime :: Int }

instance Eq Hit where
    Bang a _ == Bang b _ = a == b
    Friend a _ == Friend b _ = a == b
    Bang _ _ == Friend _ _ = False
    Friend _ _ == Bang _ _ = False

data IrcF next
    = Send Message next
    | Receive (Message -> next)
    | IsConnected (Bool -> next)
    | Log Log.Entry next
    | Fork (Free IrcF ()) next
    | Sleep Int next
    | Random (Int,Int) (Int -> next)
    | ReadD (TVar Data) (Data -> next)
    | ModifyD (TVar Data) (Data -> Data) next
    | TakeHit (MVar Hit) (Hit -> next)
    | StoreHit (MVar Hit) Hit (Bool -> next)
    | Time (POSIXTime -> next)
    | Stop

instance Functor IrcF where
    fmap f (Send m n)       = Send m (f n)
    fmap f (Receive g)      = Receive (f . g)
    fmap f (IsConnected g)  = IsConnected (f . g)
    fmap f (Log s n)        = Log s (f n)
    fmap f (Fork i n)       = Fork i (f n)
    fmap f (Sleep s n)      = Sleep s (f n)
    fmap f (Random r g)     = Random r (f . g)
    fmap f (ReadD t g)      = ReadD t (f . g)
    fmap f (ModifyD t m n)  = ModifyD t m (f n)
    fmap f (TakeHit m g)    = TakeHit m (f . g)
    fmap f (StoreHit m h g) = StoreHit m h (f . g)
    fmap f (Time g)         = Time (f . g)
    fmap f Stop             = Stop

class MonadIrcF f where
    sendF :: Message -> Free f ()
    receiveF :: Free f Message
    isConnectedF :: Free f Bool
    logF :: Log.Entry -> Free f ()
    forkF :: Free IrcF () -> Free f ()
    sleepF :: Int -> Free f ()
    randomF :: (Int,Int) -> Free f Int
    readDF :: TVar Data -> Free f Data
    modifyDF :: TVar Data -> (Data -> Data) -> Free f ()
    takeHitF :: MVar Hit -> Free f Hit
    storeHitF :: MVar Hit -> Hit -> Free f Bool
    timeF :: Free f POSIXTime
    stopF :: Free f ()

instance MonadIrcF IrcF where
    sendF message = liftF $ Send message ()
    receiveF = liftF $ Receive id
    isConnectedF = liftF $ IsConnected id
    logF entry = liftF $ Log entry ()
    forkF ircf = liftF $ Fork ircf ()
    sleepF num = liftF $ Sleep num ()
    randomF range = liftF $ Random range id
    readDF tv = liftF $ ReadD tv id
    modifyDF tv f = liftF $ ModifyD tv f ()
    takeHitF mv = liftF $ TakeHit mv id
    storeHitF mv hit = liftF $ StoreHit mv hit id
    timeF = liftF $ Time id
    stopF = liftF $ Stop

instance MonadIrcF f => MonadIrc (Free f) where
    send = sendF
    receive = receiveF
    isConnected = isConnectedF
    log = logF
    fork = forkF
    sleep = sleepF
    random = randomF
    readD = readDF
    modifyD = modifyDF
    takeHit = takeHitF
    storeHit = storeHitF
    time = timeF
    stop = stopF

class MonadIrc m where
    send :: Message -> m ()
    receive :: m Message
    isConnected :: m Bool
    log :: Log.Entry -> m ()
    fork :: Free IrcF () -> m ()
    sleep :: Int -> m ()
    random :: (Int,Int) -> m Int
    readD :: TVar Data -> m Data
    modifyD :: TVar Data -> (Data -> Data) -> m ()
    takeHit :: MVar Hit -> m Hit
    storeHit :: MVar Hit -> Hit -> m Bool
    time :: m POSIXTime
    stop :: m ()

instance (Monad m, MonadIrc m) => MonadIrc (StateT s m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    fork = lift . fork
    sleep = lift . sleep
    random = lift . random
    readD = lift . readD
    modifyD = (lift .) . modifyD
    takeHit = lift . takeHit
    storeHit = (lift .) . storeHit
    time = lift time
    stop = lift stop

instance (Monad m, MonadIrc m) => MonadIrc (ReaderT r m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    fork = lift . fork
    sleep = lift . sleep
    random = lift . random
    readD = lift . readD
    modifyD = (lift .) . modifyD
    takeHit = lift . takeHit
    storeHit = (lift .) . storeHit
    time = lift time
    stop = lift stop

instance (Monad m, Monoid w, MonadIrc m) => MonadIrc (WriterT w m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    fork = lift . fork
    sleep = lift . sleep
    random = lift . random
    readD = lift . readD
    modifyD = (lift .) . modifyD
    takeHit = lift . takeHit
    storeHit = (lift .) . storeHit
    time = lift time
    stop = lift stop

runHandler :: Free IrcF a -> ReaderT IrcConnection IO ()
runHandler (Pure a)                  = return ()
runHandler (Free (Send m n))         = asks sendQ >>=
                                       liftIO . (flip writeChan m) >>
                                       liftIO (putStrLn $ " >>= " ++ show m) >>
                                       runHandler n
runHandler (Free (Receive g))        = asks readQ >>=
                                       liftIO . readChan >>=
                                       runHandler . g
runHandler (Free (IsConnected g))    = asks connected >>=
                                       liftIO . atomically . readTVar >>=
                                       runHandler . g
runHandler (Free (Log s n))          = liftIO (Log.write s) >>
                                       runHandler n
runHandler (Free (Fork i n))         = ask >>=
                                       liftIO . forkIO . runReaderT (runHandler i) >>
                                       runHandler n
runHandler (Free (Sleep s n))        = liftIO (threadDelay (1000000*s)) >>
                                       runHandler n
runHandler (Free (Random r g))       = liftIO (randomRIO r) >>=
                                       runHandler . g
runHandler (Free (ReadD t g))        = liftIO (atomically $ readTVar t) >>=
                                       runHandler . g
runHandler (Free (ModifyD t f n))    = liftIO (atomically $ modifyTVar t f) >>
                                       liftIO (atomically $ readTVar t) >>=
                                       liftIO . writeDuckData . duckData >>
                                       runHandler n
runHandler (Free (TakeHit m g))      = liftIO (takeMVar m) >>=
                                       runHandler . g
runHandler (Free (StoreHit m h g))   = liftIO (tryPutMVar m h) >>=
                                       runHandler . g
runHandler (Free (Time g))           = liftIO getPOSIXTime >>=
                                       runHandler . g
runHandler (Free (Stop))             = return ()

register :: String -> String -> Maybe String -> StateT (TVar Data) (Free IrcF) ()
register n u p = ifM (get >>= readD >>= return . registered) (return ()) (tryRegister >> expectWelcome)
        where
            tryRegister :: StateT (TVar Data) (Free IrcF) ()
            tryRegister = send (irc_user u "Monad bot") >>
                          send (irc_nick n) >>
                          maybe (return ()) (send . irc_pass) p
            expectWelcome :: StateT (TVar Data) (Free IrcF) ()
            expectWelcome = receive >>=
                            isWelcome >>=
                            setRegistered >>
                            ifM isConnected (register n u p) (return ())
            isWelcome :: Message -> StateT (TVar Data) (Free IrcF) Bool
            isWelcome m = log (Log.info $ " =<< "++(show m)) >>
                          if command m == Numeric RPL_Welcome
                          then return True
                          else if command m == Numeric ERR_NicknameInUse
                               then (send . irc_nick $ n++"_") >> return False
                               else if command m == Ping
                                    then runReaderT (execWriterT handlePing) m >> return False
                                    else if command m == Notice
                                         then receive >>= isWelcome 
                                         else return False
            setRegistered :: Bool -> StateT (TVar Data) (Free IrcF) ()
            setRegistered val = get >>= flip modifyD (\d -> d { registered = val })

handler :: StateT (TVar Data) (Free IrcF) ()
handler = receive >>=
          runReaderT handleMessage >>
          ifM isConnected handler (return ())

handleMessage :: ReaderT Message (StateT (TVar Data) (Free IrcF)) ()
handleMessage = asks command >>=
                execWriterT . (logMessage >>) . dispatch >>=
                mapM_ log
        where
            dispatch :: Command -> WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
            dispatch Ping       = handlePing
            dispatch Privmsg    = handlePrivmsg
            dispatch Notice     = handleNotice
            dispatch Invite     = handleInvite
            dispatch _          = handleOther
            logMessage :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
            logMessage = ask >>= \msg -> tell [Log.debug $ " =<< "++(show msg)]

handlePrivmsg :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handlePrivmsg = ask >>= \msg@(Message (Just prefix) Privmsg (Short chan (Long message))) ->
                    handleCommand prefix chan message 

data BotCommand = StartHunt | Ducks | Shoot | Befriend | Killers | Friends | Unknown String

handleCommand :: Prefix -> Text -> Text -> WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handleCommand pfx chan msg 
        | T.isPrefixOf (T.singleton '.') msg = let
                                                (command, params) = T.break (==' ') . T.drop 1 $ msg
                                                name = nick pfx
                                               in
                                                case readCommand command of
                                                StartHunt -> reply "The hunt will start in a moment." >>
                                                               get >>= \tv -> 
                                                                modifyD tv (\d -> d { hunting = True }) >>
                                                                fork (evalStateT duckWorker tv)
                                                Ducks -> get >>= readD >>= \Data { duckData = dd } ->
                                                              (reply $ "You have killed "++(show $ bangCount dd name)++
                                                                       " and befriended "++(show $ friendCount dd name)++" ducks.")
                                                Shoot -> time >>= \t ->
                                                          get >>= readD >>= \Data { duckState = mv, duckTime = d, duckData = dd } ->
                                                            case d of
                                                            0 -> reply "There is no duck, what are you shooting at?"
                                                            _ -> ifM (storeHit mv (Bang { banger = name, bangtime = round t }))
                                                                  (reply $ "You shot the duck in "++(show $ t - d)++"! "++
                                                                           "You have killed "++(show $ (bangCount dd name)+1)++" ducks.")
                                                                  (reply "There is no duck, what are you shooting at?")
                                                Befriend -> time >>= \t ->
                                                            get >>= readD >>= \Data { duckState = mv, duckTime = d, duckData = dd } ->
                                                              case d of
                                                                0 -> reply "You're befriending a nonexistent duck, that's fucking creepy"
                                                                _ -> ifM (storeHit mv (Friend { friender = name, friendtime = round t }))
                                                                  (reply $ "You befriended the duck in "++(show $ t - d)++"! "++
                                                                           "You have befriended "++(show $ (friendCount dd name)+1)++" ducks.")
                                                                  (reply "You're befriending a nonexistent duck, that's fucking creepy")
                                                Killers -> get >>= readD >>= \Data { duckData = dd } ->
                                                                (lift . lift . lift) (bangRanks dd) >>=
                                                                reply . ellipsizedUnwords . ("Top killers (month/all): ":)
                                                Friends -> get >>= readD >>= \Data { duckData = dd } ->
                                                                (lift . lift . lift) (friendRanks dd) >>=
                                                                reply . ellipsizedUnwords . ("Top frienders (month/all): ":)
                                                Unknown cmd -> tell [Log.warn $ "Unknown command: "++cmd]
        | otherwise                          = return ()
        where
            reply :: String -> WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
            reply = send . irc_privmsg chan
            duckWorker :: StateT (TVar Data) (Free IrcF) ()
            duckWorker = get >>= readD >>= \Data { duckState = mv, duckData = dd } ->
                            random (1,10) >>= \sleepTime ->
                                sleep sleepTime >>
                                time >>= \dt ->
                                    lift makeDuck >>=
                                    send . irc_privmsg chan >>
                                    get >>= flip modifyD (\d -> d { duckTime = dt }) >>
                                    takeHit mv >>= \hit ->
                                        get >>= flip modifyD (\d -> d { duckTime = 0, duckData = amend dd hit }) >>
                                        ifM isConnected duckWorker (return ())
            amend :: DuckData -> Hit -> DuckData
            amend dd@(DuckData bn fr) hit@(Bang _ _) = dd { bangs = hit:bn }
            amend dd@(DuckData bn fr) hit@(Friend _ _) = dd { friends = hit:fr }
            readCommand :: Text -> BotCommand
            readCommand c = case map toLower (T.unpack c) of
                            "starthunt" -> StartHunt
                            "ducks" -> Ducks
                            "bang" -> Shoot
                            "friend" -> Befriend
                            "befriend" -> Befriend
                            "bef" -> Befriend
                            "killers" -> Killers
                            "friends" -> Friends
                            cmd -> Unknown cmd

handlePing :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handlePing = asks (param . params) >>= send . irc_pong 

handleNotice :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handleNotice = send (irc_join "##doge")

handleInvite :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handleInvite = asks (param . next . params) >>= send . irc_join 

handleOther :: WriterT [Log.Entry] (ReaderT Message (StateT (TVar Data) (Free IrcF))) ()
handleOther = return ()

startHandler :: IrcConnection -> IO ()
startHandler ic = readDuckData >>= \dd ->
                  newEmptyMVar >>= \mv ->
                  atomically (newTVar $ theData dd mv) >>= \tv ->
                    runReaderT (runHandler (theHandler tv)) ic
        where
            theHandler :: TVar Data -> Free IrcF ()
            theHandler tvar = execStateT (register "_mako" "monadbot" Nothing >> handler) tvar >>
                              return ()
            theData :: DuckData -> MVar Hit -> Data
            theData dd mv = Data { registered = False, hunting = False, duckState = mv, duckData = dd, duckTime = 0 }

startNetWorker :: IrcServer -> IO IrcConnection
startNetWorker is = connect (IrcServer.host is) (IrcServer.port is) >>=
                    runReaderT listen

makeDuck :: Free IrcF String
makeDuck = let
            duck_tail = ["\ETX04・","\ETX07゜","\ETX08゜","\ETX09・","\ETX03。","\ETX10。","\ETX12・","\ETX02゜","\ETX06゜","\x0f"]
            duck = ["\\_o< ", "\\_O< ", "\\_0< ", "\\_\x00f6< ", "\\_\x00f8< ", "\\_\x00f3< "]
            duck_noise = ["QUACK!", "FLAP FLAP!", "quack!"]
           in
            random (1,(length duck_tail) - 2) >>= \rt ->
            return (unwords $ (take rt duck_tail) ++ [" \x200b "] ++ (drop (rt+1) duck_tail)) >>= \dtail ->
            choice duck >>= \dbody ->
            random (1,(length dbody) - 1) >>= \rb ->
            return ((take rb dbody) ++ "\x200b" ++ (drop rb dbody)) >>= \dbody ->
            choice duck_noise >>= \dnoise ->
            random (1,(length dnoise) - 1) >>= \rn ->
            return ((take rn dnoise) ++ "\x200b" ++ (drop rn dnoise)) >>= \dnoise ->
            return (unwords $ [dtail, dbody, dnoise])
        where
            choice :: [a] -> Free IrcF a
            choice xs = random (1,length xs) >>= return . (\x -> xs !! (x-1))

number :: [String] -> [String]
number ss = zipWith (\n (x:xs) -> (show n)++". _"++xs) [1..length ss] ss

bangCount :: DuckData -> Text -> Int
bangCount (DuckData bn _) name = length $ filter (\b -> banger b == name) bn

friendCount :: DuckData -> Text -> Int
friendCount (DuckData _ fr) name = length $ filter (\f -> friender f == name) fr

monthAgo :: Free IrcF Int
monthAgo = time >>= return . (\x -> x - (86400*30)) . round

getNames :: [Hit] -> [Text]
getNames hits = unique . sort $ go hits
        where
            go (Bang n _:xs) = n:go xs
            go (Friend n _:xs) = n:go xs
            go [] = []
            unique (x:y:xs) = case x == y of
                                True -> unique (x:xs)
                                False -> x:unique (y:xs)
            unique other      = other

sortRanks :: [(String, Int, Int)] -> [String]
sortRanks ss = map render $ sortOn (\(n, a, b) -> a) ss
        where
            render :: (String, Int, Int) -> String
            render (n, a, b) = n++" ("++(show a)++"/"++(show b)++")"

recentBangs :: DuckData -> Text -> Free IrcF Int
recentBangs (DuckData bn _) name = fmap length . filterM (\b -> fmap (bangtime b >) monthAgo) . filter (\b -> banger b == name) $ bn

bangRanks :: DuckData -> Free IrcF [String]
bangRanks dd = fmap (number . sortRanks) . mapM findRanks . getNames $ bangs dd
        where
            findRanks :: Text -> Free IrcF (String, Int, Int)
            findRanks n = recentBangs dd n >>= \x -> return $ (T.unpack n, x, bangCount dd n)

recentFriends :: DuckData -> Text -> Free IrcF Int
recentFriends (DuckData _ fr) name = fmap length . filterM (\f -> fmap (friendtime f >) monthAgo) . filter (\f -> friender f == name) $ fr

friendRanks :: DuckData -> Free IrcF [String]
friendRanks dd = fmap (number . sortRanks) . mapM findRanks . getNames $ friends dd
        where
            findRanks :: Text -> Free IrcF (String, Int, Int)
            findRanks n = recentFriends dd n >>= \x -> return $ (T.unpack n, x, friendCount dd n)

emptyDuckData = DuckData { bangs = [], friends = [] }

readDuckData :: IO DuckData
readDuckData = readBangs >>= \b ->
               readFriends >>= \f ->
                putStrLn ("Loaded "++(show $ length b)++" bangs and "++(show $ length f)++" friends.") >>
                return (emptyDuckData {bangs = b, friends = f})

readBangs :: IO [Hit]
readBangs = catch (readFile "bangs.csv") (\(e :: IOException) -> return "") >>=
            return . map unserialize . lines
        where
            unserialize line = let (n,t) = break (==',') line
                               in Bang { banger = T.pack n, bangtime = read (drop 1 t) }

readFriends :: IO [Hit]
readFriends = catch (readFile "friends.csv") (\(e :: IOException) -> return "") >>=
              return . map unserialize . lines
        where
            unserialize line = let (n,t) = break (==',') line
                               in Friend { friender = T.pack n, friendtime = read (drop 1 t) }

writeDuckData :: DuckData -> IO ()
writeDuckData dd = writeBangs (bangs dd) >>
                   writeFriends (friends dd)

writeBangs :: [Hit] -> IO ()
writeBangs hits = writeFile "bangs.csv" $ serialize hits
        where
            serialize [] = ""
            serialize (x:xs) = (T.unpack $ banger x)++","++(show $ bangtime x)++"\n" ++ serialize xs

writeFriends :: [Hit] -> IO ()
writeFriends hits = writeFile "friends.csv" $ serialize hits
        where
            serialize [] = ""
            serialize (x:xs) = (T.unpack $ friender x)++","++(show $ friendtime x)++"\n" ++ serialize xs

ellipsizedUnwords :: [String] -> String
ellipsizedUnwords = unwords

colour :: Int -> String -> String
colour i s
        | (i > 0) && (i < 10)   = concat [[chr 3, '0'], (show i), s]
        | (i > 0) && (i <= 15)  = concat [[chr 3], (show i), s]

