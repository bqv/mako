module Main (
    main
) where

import Prelude hiding ()

import qualified Config (serverHost, serverPort, autoJoin, botNick, markovOrder, opList)
import qualified Irc (IrcServer(..), Handler, startNetWorker, startHandler, send)
import qualified Markov (imitate, catalog)
import qualified Logging as Log
import IrcMessage

import Text.Read (readMaybe)

import Control.Monad.Reader (ask, asks)
import Control.Monad.Writer.Lazy (tell)

import System.IO (IOMode(..), BufferMode(..), withFile)
import Control.Applicative ((<$>), (<*>), pure)

handlePrivmsg :: Irc.Handler ()
handlePrivmsg = ask >>= \msg ->
					tell [Log.info $ show msg] >>
					Irc.send (irc_privmsg (param $ params msg) (param . next $ params msg))

mako :: IO ()
mako = Irc.startNetWorker (Irc.IrcServer "irc.freenode.net" 6667 False) >>=
       Irc.startHandler handlePrivmsg

main :: IO ()
main = mako
