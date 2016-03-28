import Prelude hiding ()

import Network (connectTo, PortID(PortNumber))
import Debug.Trace (trace, traceShow, traceShowId)

import Text.Read (readMaybe)
import Text.Printf (hPrintf)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad (foldM, filterM, liftM, mapM)
import Control.Monad.State.Strict (StateT, State, get, put, modify, gets,
                            state, liftIO, lift, evalStateT, evalState)
import Control.Comonad (liftW, extend)
import Control.Monad.Free (Free(..), liftF)

import System.Random (randomR, newStdGen, StdGen, Random)
import System.Directory (getDirectoryContents, doesFileExist)
import System.IO (Handle, hSetBuffering, hGetLine, BufferMode(..), IOMode(..),
                  mkTextEncoding, withFile, hSetEncoding, stdout)

import Data.Maybe (catMaybes)
import Data.Char (toLower, chr)
import Data.Monoid (mappend, mconcat)
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.List as List
                            (isPrefixOf, elem, intersperse, sort, group,
                             intercalate, map)
import qualified Data.ByteString.Builder as B
                            (Builder, intDec, int8, char8, string8,
                             byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as B
                            (ByteString, singleton, pack, unpack,
                             hGetContents, hPutStr, hPutStrLn, hGetLine,
                             lines, words, unwords, intercalate, concat,
                             dropWhile, filter, cons, append, drop, take,
                             init, tail, split, length, isPrefixOf)
import qualified Data.Map.Strict as Map (Map, empty, member, findWithDefault, fromList, (!), 
                     unions, keys, insertWith, unionWith, toList,
                     mapWithKey, fromListWith, foldr, elems, size)
import qualified Data.HashMap as H (Map, empty, member, findWithDefault, fromList, (!), 
                     unions, keys, insertWith, unionWith, toList,
                     mapWithKey, fromListWith, fold , elems, size)

server          = "irc.sublumin.al"
port            = 6667
autojoinChan    = ["#spam"]
nick            = "mako"
markovOrder     = 2
opList          = Map.fromList . concat -- Didn't happen >_<
                               . map (\(a,b) -> map (flip (,) b . (:a)) ".!@") $
                                                    [("imitate", imitate),
                                                     ("im", imitate),
                                                     ("speak", imitateall),
                                                     ("imitateall", imitateall)]

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

main :: IO ()
main    = runImitateIO (prepare >> connect >>= listen)

prepare :: ImitateIO ()
prepare = liftIO (
            hSetBuffering stdout LineBuffering ) >>
          lift loadLogs >>
          return ()

connect :: ImitateIO Handle
connect = liftIO ( (connectTo server . PortNumber . fromIntegral $ port) >>=
          \h ->
            mkTextEncoding "UTF-8//IGNORE" >>=
            hSetEncoding h >>
            hSetBuffering h NoBuffering >>
            write h "USER" "markovbot 0 * :Markov bot" >>
            write h "NICK" nick >>
            return h )

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

write :: Handle -> String -> String -> IO ()
write h s t     = hPrintf h "%s %s\r\n" s t >>
                  mapM_ putStr ["=>> ", s, " ", t, "\n"]

listen :: Handle -> ImitateIO ()
listen h    = liftIO (hGetLine h) >>=
              \t -> let
                      s = init t
                    in
                      liftIO (putStrLn s) >>
                      if isPing s then
                          liftIO (doPong s) >> listen h
                      else
                          parseLine h (words s) >> listen h
        where
            isPing  = List.isPrefixOf "PING :"
            doPong  = write h "PONG" . (':':) . drop 6

joinChan :: Handle -> [String] -> IO ()
joinChan h (x:xs)   = write h "JOIN" x >> joinChan h xs
joinChan _ []       = return ()

privmsg :: Handle -> String -> String -> IO ()
privmsg h c s = write h "PRIVMSG" $ concat [c, " :", s]

notice :: Handle -> String -> String -> IO ()
notice h c s = write h "NOTICE" $ concat [c, " :", s]

parseLine :: Handle -> [String] -> ImitateIO ()
parseLine h (prefix:command:params)
        | command == "001"      = liftIO ( write h "UMODE2" "+B" >>
                                           joinChan h autojoinChan )
        | command == "INVITE"   = liftIO ( joinChan h (tail params) )
        | command == "PRIVMSG"  = handleMsg h prefix params
        | otherwise             = return () -- Ignore
parseLine _ _                   = liftIO ( putStrLn "Couldn't parse line" )

handleMsg :: Handle -> String -> [String] -> ImitateIO ()
handleMsg h prefix (chan:(_:operation):args)
        | Map.member op opList  = liftIO (  mapM_ putStr [senderNick
                                                        ," called "
                                                        ,(concat [op
                                                                ,"('"
                                                                ,List.intercalate "','" args
                                                                ,"')"])
                                                        ," in "
                                                        ,chan
                                                        ,"\n"] ) >> 
                                  imIO ((opList Map.! op) args) >>=
                                  liftIO . ( notice h chan )
        | otherwise         = liftIO (  mapM_ putStr [senderNick
                                                    ," said "
                                                    ,(List.intercalate " " $ operation:args)
                                                    ," in "
                                                    ,chan
                                                    ,"\n"] ) >>
                              imIO ( lift ( catalog (B.pack senderNick) (List.map B.pack (operation:args)) ) )
        where
            op          = List.map toLower operation
            senderNick  = takeWhile (\c -> (c /='@') && (c /='!')) $ tail prefix
handleMsg _ _ _             = liftIO ( putStrLn "Couldn't handle message" )

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

