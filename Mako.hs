module Mako (
    main
) where

import Prelude hiding ()

import qualified Config (serverHost, serverPort, autoJoin, botNick, markovOrder, opList)
import qualified Irc (IrcServer(..), startWorker)
import qualified Markov ()

import Control.Monad.Free (Free(..), liftF)

import Text.Read (readMaybe)

import System.IO (IOMode(..), BufferMode(..), withFile)
import Control.Applicative ((<$>), (<*>), pure)

main :: IO ()
main = return ()
