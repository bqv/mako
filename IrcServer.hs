module IrcServer (
    connect,
    listen,
    work,
    parseLine
) where

import Config

import Prelude hiding ()

import IrcMessage

import Network (connectTo, PortID(PortNumber))

import System.IO (Handle, hSetBuffering, BufferMode(..), hGetLine,
                  mkTextEncoding, hSetEncoding, stdout, hSetNewlineMode,
                  NewlineMode(..), Newline(..))

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan (Chan(..), newChan, writeChan, readChan, getChanContents)
import Control.Concurrent.MVar (MVar(..), newMVar, readMVar, putMVar)

import Control.Exception (IOException, handle)

import Control.Monad (forever)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask)

connect :: String -> Int -> IO Handle
connect server port = connectTo server portno >>=
                      \h ->
                        mkTextEncoding "UTF-8//IGNORE" >>=
                        hSetEncoding h >>
                        hSetBuffering h NoBuffering >>
                        hSetNewlineMode h nlCRLFMode >>
                        return h
        where
            portno = PortNumber . fromIntegral $ port
            nlCRLFMode = NewlineMode CRLF CRLF

register :: Handle -> String -> String -> Maybe String -> IO ()
register h n u p = write h (irc_user u "Monad bot") >>
                   write h (irc_nick n) >>
                   maybe (return ()) (write h . irc_pass) p

listen :: Handle -> IO (Chan Message, Chan Message)
listen h =  register h "bot" "monadbot" Nothing >>
            makeChans >>= \(inp, out) ->
              forkIO (forever $ hGetLine h >>=
                                writeOut inp >>
                                readIn out) >>
              return (inp, out)
        where
            writeOut :: Chan Message -> String -> IO ()
            writeOut inp st = putStrLn st >>
                              writeChan inp (read st)
            readIn :: Chan Message -> IO ()
            readIn out = getChanContents out >>=
                         mapM_ (write h)
            makeChans :: IO (Chan Message, Chan Message)
            makeChans = newChan >>= \input ->
                        newChan >>= \output ->
                        return (input, output)

work :: Handle -> (Message -> Writer [Message] ()) -> IO ()
work h fn = register h "bot" "monadbot" Nothing >>
            safely (forever $ hGetLine h >>= fork) >>
            return ()
        where
            safely :: IO () -> IO ()
            safely = handle (print :: IOException -> IO ())
            fork :: String -> IO ThreadId
            fork st = forkIO $ putStrLn st >>
                               (mapM_ (write h) $ execWriter . fn $ read st)

type Net = ReaderT Handle IO

data IrcConnection = IrcConnection {
                        readQ :: Chan Message,
                        sendQ :: Chan Message
                     }

type IRC = ReaderT IrcConnection IO



