module Irc (
    module Irc,
    IrcServer(..)
) where

import Prelude hiding (log)

import IrcServer (IrcServer(..), IrcConnection(..), connect, listen)
import IrcMessage
import qualified Logging as Log

import qualified Data.Text as T (Text(..), pack, unpack)
import Data.Char (chr)

import Control.Concurrent.Chan (writeChan, readChan, getChanContents)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar(..), newTVar, readTVar, writeTVar)

import Control.Monad.State (StateT(..), execStateT, get, put, gets, modify)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask, asks, reader, liftIO)
import Control.Monad.Writer.Lazy (WriterT(..), execWriterT, tell)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Trans (lift)

import Control.Conditional (if', ifM)

data Data = Data { registered :: Bool }

data IrcF next
    = Send Message next
    | Receive (Message -> next)
    | IsConnected (Bool -> next)
    | Log Log.Entry next
    | Stop

instance Functor IrcF where
    fmap f (Send m n)       = Send m (f n)
    fmap f (Receive g)      = Receive (f . g)
    fmap f (IsConnected g)  = IsConnected (f . g)
    fmap f (Log s n)        = Log s (f n)
    fmap f Stop             = Stop

class MonadIrcF f where
    sendF :: Message -> Free f ()
    receiveF :: Free f Message
    isConnectedF :: Free f Bool
    logF :: Log.Entry -> Free f ()
    stopF :: Free f ()

instance MonadIrcF IrcF where
    sendF message = liftF $ Send message ()
    receiveF = liftF $ Receive id
    isConnectedF = liftF $ IsConnected id
    logF entry = liftF $ Log entry ()
    stopF = liftF $ Stop

instance MonadIrcF f => MonadIrc (Free f) where
    send = sendF
    receive = receiveF
    isConnected = isConnectedF
    log = logF
    stop = stopF

class MonadIrc m where
    send :: Message -> m ()
    receive :: m Message
    isConnected :: m Bool
    log :: Log.Entry -> m ()
    stop :: m ()

instance (Monad m, MonadIrc m) => MonadIrc (StateT s m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    stop = lift stop

instance (Monad m, MonadIrc m) => MonadIrc (ReaderT r m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    stop = lift stop

instance (Monad m, Monoid w, MonadIrc m) => MonadIrc (WriterT w m) where
    send = lift . send
    receive = lift receive
    isConnected = lift isConnected
    log = lift . log
    stop = lift stop

runHandler :: Free IrcF a -> ReaderT IrcConnection IO ()
runHandler (Pure a)                  = return ()
runHandler (Free (Send m n))         = asks sendQ >>=
                                       liftIO . (flip writeChan m) >>
                                       runHandler n
runHandler (Free (Receive g))        = asks readQ >>=
                                       liftIO . readChan >>=
                                       runHandler . g
runHandler (Free (IsConnected g))    = asks connected >>=
                                       liftIO . atomically . readTVar >>=
                                       runHandler . g
runHandler (Free (Log s n))          = liftIO (Log.write s) >>
                                       runHandler n
runHandler (Free (Stop))             = return ()

register :: String -> String -> Maybe String -> StateT Data (Free IrcF) ()
register n u p = ifM (gets registered) (return ()) (tryRegister >> expectWelcome)
        where
            tryRegister :: StateT Data (Free IrcF) ()
            tryRegister = send (irc_user u "Monad bot") >>
                          send (irc_nick n) >>
                          maybe (return ()) (send . irc_pass) p
            expectWelcome :: StateT Data (Free IrcF) ()
            expectWelcome = receive >>=
                            isWelcome >>=
                            setRegistered >>
                            ifM isConnected (register n u p) (return ())
            isWelcome :: Message -> StateT Data (Free IrcF) Bool
            isWelcome m = if command m == Numeric RPL_Welcome
                          then return True
                          else if command m == Numeric ERR_NicknameInUse
                               then (send . irc_nick $ n++"_") >> return False
                               else if command m == Ping
                                    then runReaderT (execWriterT handlePing) m >> return False
                                    else return False
            setRegistered :: Monad m => Bool -> StateT Data m ()
            setRegistered val = modify (\d -> d { registered = val })

handler :: StateT Data (Free IrcF) ()
handler = receive >>=
          runReaderT handleMessage >>
          ifM isConnected handler (return ())

handleMessage :: ReaderT Message (StateT Data (Free IrcF)) ()
handleMessage = asks command >>=
                execWriterT . dispatch >>=
                mapM_ log
        where
            dispatch :: Command -> WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
            dispatch Ping       = handlePing
            dispatch Privmsg    = handlePrivmsg
            dispatch Notice     = handleNotice
            dispatch _          = handleOther

handlePing :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handlePing = asks params >>= send . irc_pong . T.unpack . param

handlePrivmsg :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handlePrivmsg = ask >>= \msg -> tell [Log.info $ show msg]

handleNotice :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handleNotice = ask >>= \msg -> tell [Log.info $ show msg] >>
               (send $ irc_join "#botters")

handleOther :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handleOther = ask >>= \msg -> tell [Log.debug $ " =<< "++(show msg)]

startHandler :: IrcConnection -> IO ()
startHandler ic = runReaderT (runHandler theHandler) ic
        where
            theHandler :: Free IrcF ()
            theHandler = execStateT (register "bot" "monadbot" Nothing >> handler) theData >> return ()
            theData :: Data
            theData = Data { registered = False }

startNetWorker :: IrcServer -> IO IrcConnection
startNetWorker is = connect (IrcServer.host is) (IrcServer.port is) >>=
                    runReaderT listen

colour :: Int -> String -> String
colour i s
        | (i > 0) && (i < 10)   = concat [[chr 3, '0'], (show i), s]
        | (i > 0) && (i <= 15)  = concat [[chr 3], (show i), s]

