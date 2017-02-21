module Config (
    serverHost,
    serverPort,
    autoJoin,
    botNick,
    markovOrder,
    opList
) where

import Prelude hiding ()

--import Main (imitate, imitateall)

import qualified Data.List as List (concat, map, concatMap)
import qualified Data.Map as Map (Map(..), fromList)

type Map  = Map.Map
imitate    = undefined
imitateall = undefined

serverHost :: String
serverHost  = "irc.sublumin.al"

serverPort :: Int
serverPort  = 6667

autoJoin :: [String]
autoJoin    = ["#spam"]

botNick :: String
botNick     = "mako"

markovOrder :: Int
markovOrder = 2

opList :: Map String f
opList      = Map.fromList . List.concatMap ( \(a,b) -> -- Didn't happen >_<
                                                List.map (flip (,) b . (:a)) ".!@" ) $
                                            [("imitate", imitate),
                                             ("im", imitate),
                                             ("speak", imitateall),
                                             ("imitateall", imitateall)]
