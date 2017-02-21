module Mako (
    main
) where

import Prelude hiding ()

import qualified Config (serverHost, serverPort, autoJoin, botNick, markovOrder, opList)
import qualified IrcServer as Irc (connect, listen)

import Control.Monad.Free (Free(..), liftF)

import Debug.Trace (trace, traceShow, traceShowId)

import Text.Read (readMaybe)

import System.IO (IOMode(..), BufferMode(..), withFile)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad (foldM, filterM, liftM, mapM)
import Control.Monad.State.Strict (StateT, State, get, put, modify, gets,
                            state, liftIO, lift, evalStateT, evalState)
import Control.Comonad (liftW, extend)

import System.Random (randomR, newStdGen, StdGen, Random)
import System.Directory (getDirectoryContents, doesFileExist)

import Data.Maybe (catMaybes)
import Data.Char (toLower, chr)
import Data.Monoid (mappend, mconcat)
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.HashMap as H (Map, empty, member, findWithDefault, fromList, (!), 
                     unions, keys, insertWith, unionWith, toList,
                     mapWithKey, fromListWith, fold , elems, size)

data BotActionF next
    = Prepare next
    | Connect host port (sock -> next)
    | Listen sock next
    | Stop
    deriving (Eq, Show)

instance Functor BotActionF where
    fmap f (Prepare n)      = Prepare (f n)
    fmap f (Connect h p g)  = Connect h p (f . g)
    fmap f (Listen s n)     = Listen s (f n)
    fmap f Stop             = Stop

type BotAction  = Free BotActionF

prepare :: BotAction ()
prepare = liftF $ Prepare ()

connect :: String -> Int -> BotAction socket
connect host port = liftF $ Connect host port id

listen :: socket -> BotAction ()
listen handle = liftF $ Listen handle ()

stop :: BotAction ()
stop = liftF $ Stop

runBot :: BotAction a -> IO a
runBot (Pure a)               = return a
runBot (Free (Prepare n))     = hSetBuffering stdout LineBuffering >>
                                forkIO Markov.runMarkov
                                runBot n
runBot (Free (Connect h p g)) = Irc.connect h p >>= 
                                runBot . g
runBot (Free (Listen s n))    = Irc.listen s >>=
                                runBot n
runBot (Free (Stop))          = return ()

mako :: BotAction ()
mako  = prepare >>
        connect Config.serverHost Config.serverIP >>=
        listen >>
        stop

main :: IO ()
main  = runBot mako

-- Code

type Map = Map.Map
type ByteString = B.ByteString
type HashMap = H.Map

data Index keyType = All | Name keyType deriving (Eq)

instance Ord a => Ord (Index a) where
    All <= All = True
    All <= Name a = True
    Name a <= Name b = a <= b
    _ <= _ = False

instance Hashable a => Hashable (Index a) where
    hashWithSalt s All = hashWithSalt s ()
    hashWithSalt s (Name a) = hashWithSalt s a

type FrequencyMap wordType = Map (Maybe wordType) Integer

type MarkovChain wordType = Map [Maybe wordType] (FrequencyMap wordType)

type ChainList keyType wordType = Map (Index keyType) (MarkovChain wordType)


type Rand = State StdGen
type RandT = StateT StdGen

type Markov = State (ChainList ByteString ByteString)
type MarkovT = StateT (ChainList ByteString ByteString)

type Imitate a = RandT Markov a
type ImitateIO a = RandT (MarkovT IO) a

runImitateIO :: ImitateIO a -> IO a
runImitateIO im = newStdGen >>= \gen ->
                      evalStateT (evalStateT im gen) Map.empty

imIO :: Imitate a -> ImitateIO a
imIO    = hoist $ hoist generalize

loadLogs :: MarkovT IO [[()]]
loadLogs    = let
                folders = [ "/srv/log/subluminal/#programming/" ]
              in
                liftIO (
                    (fmap concat . sequence . List.map getDir $ folders) >>=
                    filterM doesFileExist ) >>= 
                mapM importLog
            where
                getDir :: FilePath -> IO [FilePath]
                getDir p = fmap (List.map (++) [p] <*>)
                                $ getDirectoryContents p

importLog :: FilePath -> MarkovT IO [()]
importLog path  = liftIO (
                    mapM_ putStr ["Loading ", path, "...\n"] >>
                    withFile path ReadMode B.hGetContents >>=
                    return . List.map processLogLine . B.lines ) >>=
                  \logs ->
                        hoist generalize ((mapM . uncurry $! catalog) logs)

processLogLine :: ByteString -> (ByteString, [ByteString])
processLogLine line = let
                        entry = tail . B.words $ line
                        nick = stripNick . head $ entry
                        msg = tail $ entry
                      in
                        (nick, msg)
            where
                stripNick :: ByteString -> ByteString
                stripNick s = B.drop 1 $ B.take (B.length s - 1) s
                --stripNick = B.dropWhile (flip elem "<0123456789")
                --     . B.filter (not . flip elem "\ETX>")

readUTF8File :: FilePath -> IO ByteString
readUTF8File path = withFile path ReadMode (\fd ->
                        mkTextEncoding "UTF-8//IGNORE" >>=
                        hSetEncoding fd >>
                        B.hGetContents fd )

-- Pure

catalog :: ByteString -> [ByteString] -> Markov ()
catalog user []      = return ()
catalog user words   = let
                         chainDiff = fromWalk stripMsg words
                         key = Name user
                         updateChain = Map.insertWith markovAdd key chainDiff
                         updateAll = Map.insertWith markovAdd All chainDiff
                       in
                         modify (updateChain . updateAll)
                   where
                       stripMsg :: ByteString -> ByteString
                       stripMsg s = s

markovAdd :: (Hashable word, Ord word) => MarkovChain word -> MarkovChain word -> MarkovChain word
markovAdd large small   = Map.unionWith freqAdd large small

freqAdd :: (Hashable word, Ord word) => FrequencyMap word -> FrequencyMap word -> FrequencyMap word
freqAdd large small = Map.unionWith (+) large small

fromWalk :: (Hashable word, Ord word) => (word -> word) -> [word] -> MarkovChain word
fromWalk _ []       = Map.empty
fromWalk wrdfltr w  = Map.mapWithKey (\k -> Map.fromList) $ Map.fromListWith (++) (transitionCount $ toTransitions wrdfltr $ w)

transitionCount :: (Hashable word, Ord word) => [[Maybe word]] -> [([Maybe word], [(Maybe word, Integer)])]
transitionCount transitions = List.map collate (List.group (List.sort transitions))
                where
                    collate :: [[word]] -> ([word], [(word, Integer)])
                    collate l = (init $ head l, [(last $ head l, toInteger $ length l)])

toTransitions :: (word -> word) -> [word] -> [[Maybe word]]
toTransitions strip l   = helper $ (replicate (markovOrder) Nothing) ++ map (Just . strip) l ++ [Nothing]
        where
            helper :: [Maybe word] -> [[Maybe word]]
            helper wl 
                | length wl > markovOrder = (take (markovOrder + 1) wl) : (helper $ tail wl)
                | otherwise = []

-- Case;
--  0 seeds:
--      Empty list -> Return Nothing ✔
--      Otherwise  -> Random from list keys ✔
--  N seeds:
--      Empty list -> Return Nothing ✔
--      Otherwise  -> Select from list keys where max intersection 

imitateall :: [String] -> Imitate String
imitateall seeds    = lift get >>= (\chains ->
                        hoist generalize . runImitate (map B.pack seeds) $
                            Map.findWithDefault Map.empty All chains ) >>=
                      return . colour 14 . B.unpack

imitate :: [String] -> Imitate String
imitate []  = return $ colour 4 "...must construct additional pylons..."
imitate (user:seeds)    = lift get >>= (\chains ->
                            hoist generalize . runImitate (map B.pack seeds) $
                                case (readMaybe user :: Maybe Int) of
                                    Nothing ->
                                        let
                                          key = Name $ B.pack user
                                        in
                                          Map.findWithDefault Map.empty key chains
                                    Just i  ->
                                      if i >= 0 && i < Map.size chains then
                                        (!! i) $ Map.elems chains
                                      else
                                        let
                                          key = Name $ B.pack user
                                        in
                                          Map.findWithDefault Map.empty key chains) >>=
                          return . ( colour 10 . B.unpack ) >>=
                          return . (\s -> concat ["<", user, "> ", s])

runImitate :: [ByteString] -> MarkovChain ByteString -> Rand ByteString
runImitate seeds chain
        | chain == Map.empty    = return $ B.singleton '-'
        | otherwise         = startWalk chain 40 seeds >>=
                              return . B.unwords . catMaybes

startWalk :: (Hashable word, Ord word, Show word) => MarkovChain word -> Int -> [word] -> Rand [Maybe word]
startWalk chain _ _
        | chain == Map.empty    = error "(startWalk) This shouldn't happen"
startWalk chain n []        = choose (Map.keys chain) >>= \step ->
                                walk chain n step >>=
                                return . (++) step
startWalk chain n seeds     = let
                                keyList = Map.keys chain
                                candidates = findMaxSetIntersectionList
                                                    (List.map Just seeds)
                                                    keyList
                              in
                                choose candidates >>= \step ->
                                    walk chain n step >>=
                                    return . (++) step

walk :: (Hashable word, Ord word, Show word) => MarkovChain word -> Int -> [Maybe word] -> Rand [Maybe word]
walk chain i last
        | i > 0     = step chain last >>= \next ->
                          trace ("Step from "++(show last)++" to "++(show next)) $
                          if next == Nothing then
                            return [Nothing]
                          else
                            walk chain (i-1) (tail last ++ [next]) >>= 
                            return . (next :)
        | i == 0    = return last

step :: (Hashable word, Ord word) => MarkovChain word -> [Maybe word] -> Rand (Maybe word)
step chain current = let
                        freqMap = Map.findWithDefault (Map.fromList [(Nothing, 1)]) current chain
                     in
                        sample freqMap 

findMaxSetIntersectionList :: (Eq a) => [a] -> [[a]] -> [[a]]
findMaxSetIntersectionList [] setList = setList
-- stableSet: [1, 2, 4, 8, 16] 'x'
-- setList: [[1,2,3],[2,4,6],[3,6,9]] 'y'
findMaxSetIntersectionList stableSet setList =
                      let
                        -- setMatches: [[[1,2,3]],[[1,2,3],[2,4,6]],[[2,4,6]],[],[]] O(xy)
                        setMatches = map (\x -> filter (elem x) setList) stableSet
                        -- success: [True, True, True, False, False] O(x)
                        success = map (/= []) setMatches
                      in if any (/= []) setMatches then 
                            -- concat: [[2,3],[1,3],[4,6],[2,6]] 
                            concat setMatches
                         else
                            setList -- error "(findMaxSetIntersectionList) This shouldn't happen"

sample :: FrequencyMap word -> Rand (Maybe word)
sample choiceMap    = (state . randomR) (0, total-1) >>=
                      return . search (Map.toList choiceMap)
        where
            total = Map.foldr (+) 0 choiceMap
            search mapList val = case mapList of
                            ((a,b):xs)  -> if val >= b then search xs (val - b) else a
                            []          -> error "(sample) This shouldn't happen"

choose :: [a] -> Rand a
choose []       = error "(choose) This shouldn't happen"
choose choices  = (state . randomR) (0, length choices - 1) >>=
                  return . (choices !!)

-- Util

colour :: Int -> String -> String
colour i s
        | (i > 0) && (i < 10)   = concat [[chr 3, '0'], (show i), s]
        | (i > 0) && (i <= 15)  = concat [[chr 3], (show i), s]

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy del str = helper del str []   
  where 
    helper :: Eq a => a -> [a] -> [a] -> [[a]]
    helper _ [] acc = [acc] 
    helper del (x:xs) acc   
        | x == del  = acc : helper del xs []  
        | otherwise = let acc0 = acc ++ [x] in helper del xs acc0 

takeSplit :: Eq a => Int -> a -> [a] -> [[a]]
takeSplit num del str = helper num del str []   
  where 
    helper :: Eq a => Int -> a -> [a] -> [a] -> [[a]]
    helper n _ [] acc = [acc] 
    helper n del (x:xs) acc   
        | n == 0    = [x:xs]
        | x == del  = acc : helper (n - 1) del xs []
        | otherwise = let acc0 = acc ++ [x] in helper n del xs acc0 

