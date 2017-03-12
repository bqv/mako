module Main (
    main
) where

import Prelude hiding ()

import qualified Config (serverHost, serverPort, autoJoin, botNick, markovOrder, opList)
import qualified Irc (IrcServer(..), startNetWorker, startHandler)
import qualified Logging as Log

mako :: IO ()
mako = Irc.startNetWorker (Irc.IrcServer "irc.freenode.net" 6667 False) >>=
       Irc.startHandler 

main :: IO ()
main = mako
