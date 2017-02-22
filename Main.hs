module Main (
    main
) where

import Prelude hiding ()

import qualified Config (serverHost, serverPort, autoJoin, botNick, markovOrder, opList)
import qualified Irc (IrcServer(..), startWorker, startHandler)
import qualified Markov ()

import Control.Monad.Free (Free(..), liftF)

import Text.Read (readMaybe)

import System.IO (IOMode(..), BufferMode(..), withFile)
import Control.Applicative ((<$>), (<*>), pure)

mako :: IO ()
mako = Irc.startWorker (Irc.IrcServer "irc.sublumin.al" 6667 False) >>=
       Irc.startHandler

main :: IO ()
main = mako
