module Irc (
    module Irc,
    IrcServer(..)
) where

import IrcServer (IrcServer(..), IrcConnection(..), connect, register, listen)
import IrcMessage
import Logging

import Data.Char (chr)

import Control.Concurrent.Chan (writeChan, readChan, getChanContents)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar(..), newTVar, readTVar, writeTVar)

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, reader, liftIO)
import Control.Monad.Writer.Lazy (WriterT(..), execWriter, tell)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Trans (lift)

data IrcF next
    = Send Message next
    | Receive (Message -> next)
    | IsConnected (Bool -> next)
    | Stop

instance Functor IrcF where
    fmap f (Send m n)       = Send m (f n)
    fmap f (Receive g)      = Receive (f . g)
    fmap f (IsConnected g)  = IsConnected (f . g)
    fmap f Stop             = Stop

send :: Message -> Free IrcF ()
send message = liftF $ Send message ()

receive :: Free IrcF Message
receive = liftF $ Receive id

isConnected :: Free IrcF Bool
isConnected = liftF $ IsConnected id

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
runHandler (Free (Stop))             = return ()

startWorker :: IrcServer -> IO IrcConnection
startWorker is = connect (IrcServer.host is) (IrcServer.port is) >>=
                 runReaderT (register "bot" "monadbot" Nothing >> listen)

handle :: IrcConnection -> WriterT [LogMessage] (ReaderT IrcConnection (Free IrcF)) ()
handle = const $ return ()

colour :: Int -> String -> String
colour i s
        | (i > 0) && (i < 10)   = concat [[chr 3, '0'], (show i), s]
        | (i > 0) && (i <= 15)  = concat [[chr 3], (show i), s]

