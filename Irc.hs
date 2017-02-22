module Irc (
    module Irc,
    IrcServer(..)
) where

import Prelude hiding (log)

import IrcServer (IrcServer(..), IrcConnection(..), connect, listen)
import IrcMessage
import qualified Logging as Log

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

send :: Message -> Free IrcF ()
send message = liftF $ Send message ()

receive :: Free IrcF Message
receive = liftF $ Receive id

isConnected :: Free IrcF Bool
isConnected = liftF $ IsConnected id

log :: Log.Entry -> Free IrcF ()
log entry = liftF $ Log entry ()

stop :: Free IrcF ()
stop = liftF $ Stop

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

startWorker :: IrcServer -> IO IrcConnection
startWorker is = connect (IrcServer.host is) (IrcServer.port is) >>=
                 runReaderT listen

data Data = Data { registered :: Bool }

register :: String -> String -> Maybe String -> StateT Data (Free IrcF) ()
register n u p = ifM (gets registered) (lift tryRegister >> expectWelcome) (return ())
        where
            tryRegister :: Free IrcF ()
            tryRegister = send (irc_user u "Monad bot") >>
                          send (irc_nick n) >>
                          maybe (return ()) (send . irc_pass) p
            expectWelcome :: StateT Data (Free IrcF) ()
            expectWelcome = lift receive >>=
                            return . (== Numeric Welcome) . command >>=
                            setRegistered >>
                            register n u p
            setRegistered :: Bool -> StateT Data (Free IrcF) ()
            setRegistered val = modify (\d -> d { registered = val })

handler :: StateT Data (Free IrcF) ()
handler = lift receive >>=
          runReaderT handleMessage >>
          ifM (lift isConnected) handler (return ())

handleMessage :: ReaderT Message (StateT Data (Free IrcF)) ()
handleMessage = asks command >>=
                execWriterT . dispatch >>=
                lift . lift . mapM_ log
        where
            dispatch :: Command -> WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
            dispatch Privmsg    = handlePrivmsg
            dispatch _          = handleOther

handlePrivmsg :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handlePrivmsg = lift ask >>= \msg -> tell [Log.info $ show msg]

handleOther :: WriterT [Log.Entry] (ReaderT Message (StateT Data (Free IrcF))) ()
handleOther = lift ask >>= \msg -> tell [Log.debug $ show msg]

startHandler :: IrcConnection -> IO ()
startHandler ic = runReaderT (runHandler theHandler) ic
        where
            theHandler :: Free IrcF ()
            theHandler = execStateT (register "bot" "monadbot" Nothing >> handler) theData >> return ()
            theData :: Data
            theData = Data { registered = False }

colour :: Int -> String -> String
colour i s
        | (i > 0) && (i < 10)   = concat [[chr 3, '0'], (show i), s]
        | (i > 0) && (i <= 15)  = concat [[chr 3], (show i), s]

