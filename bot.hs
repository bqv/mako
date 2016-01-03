import Network (connectTo, PortID(PortNumber))
import Debug.Trace (trace, traceShow, traceShowId)

import Text.Read (readMaybe)
import Text.Printf (hPrintf, printf)

import Control.DeepSeq (force)
import Control.Applicative ()
import Control.Lens.Tuple (_1)
import Control.Lens ((&), (%~), over)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad (foldM, ap, filterM, liftM, mapM)
import Control.Monad.State.Strict (StateT, State, get, put, modify, gets,
                            state, liftIO, lift, evalStateT, evalState)

import System.Random (randomR, newStdGen, StdGen, Random)
import System.Directory (getDirectoryContents, doesFileExist)
import System.IO (Handle, hSetBuffering, hGetLine, BufferMode(..), IOMode(..),
                  mkTextEncoding, openFile, hSetEncoding, hGetContents)

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (isPrefixOf, elem, intersperse, sort, group, delete)
import Data.Map (Map, empty, member, findWithDefault, fromList, (!), 
                 unions, mapKeys, keys, insertWith, unionWith, toList,
                 mapWithKey, fromListWith, fold, elemAt, size)

server          = "irc.sublumin.al"
port            = 6667
autojoinChan    = ["#spam"]
nick            = "mako"
markovOrder     = 2
opList          = unions (map (\f ->
                                    mapKeys f . fromList $
                                                  [("imitate", imitate),
                                                   ("im", imitate),
                                                   ("speak", imitateall),
                                                   ("imitateall", imitateall)])
                              (map (:) ".!@"))

data Index keyType = All | Name keyType deriving (Eq)

instance Ord a => Ord (Index a) where
    All <= All = True
    All <= Name a = True
    Name a <= Name b = a <= b
    _ <= _ = False

type FrequencyMap wordType = Map (Maybe wordType) Integer

type MarkovChain wordType = Map [Maybe wordType] (FrequencyMap wordType)

type ChainList keyType wordType = Map (Index keyType) (MarkovChain wordType)


type Rand = State StdGen
type RandT = StateT StdGen

type Markov = State (ChainList String String)
type MarkovT = StateT (ChainList String String)

main :: IO ()
main    = newStdGen >>= \gen ->
              evalStateT (evalStateT (lift loadLogs >> connect) gen) empty

connect :: RandT (MarkovT IO) ()
connect = liftIO ( (connectTo server . PortNumber . fromIntegral $ port) >>=
          \h -> trace "Connecting..."
            hSetBuffering h NoBuffering >>
            write h "USER" "markovbot 0 * :Markov bot" >>
            write h "NICK" nick >>
            return h ) >>=
          listen

loadLogs :: MarkovT IO [[()]]
loadLogs    = let
                folders = [ "/srv/log/subluminal/#programming/" ]
              in
                liftIO (
                    (fmap concat . sequence . map getDir $ folders) >>=
                    filterM doesFileExist ) >>= trace "Loading done."
                mapM importLog
            where
                getDir :: FilePath -> IO [FilePath]
                getDir p = fmap (fmap (++) [p] `ap`)
                                $ getDirectoryContents p

importLog :: FilePath -> MarkovT IO [()]
importLog path  = liftIO (
                    printf "Loading %s...\n" path >>
                    readUTF8File path >>=
                    return . map processLogLine . lines ) >>=
                  \logs ->
                        hoist generalize ((mapM . uncurry $ catalog) logs)

processLogLine :: String -> (String, [String])
processLogLine line = let
                        entry = takeSplit 2 ' ' line
                        nick = stripNick (entry !! 1)
                        msg = splitBy ' ' (entry !! 2)
                      in
                        (nick, msg)
            where
                stripNick :: String -> String
                stripNick = dropWhile (flip elem "<0123456789")
                     . filter (not . flip elem "\ETX>")

readUTF8File :: FilePath -> IO String
readUTF8File path = openFile path ReadMode >>= \fd ->
                        mkTextEncoding "UTF-8//IGNORE" >>=
                        hSetEncoding fd >>
                        hGetContents fd 

write :: Handle -> String -> String -> IO ()
write h s t     = hPrintf h "%s %s\r\n" s t >>
                  printf "=>> %s %s\n" s t

listen :: Handle -> RandT (MarkovT IO) ()
listen h    = liftIO (hGetLine h) >>=
              \t -> let
                      s = init t
                    in
                      liftIO (putStrLn s) >>
                      if isPing s then
                          liftIO (doPong s) >> listen h
                      else
                          parseLine h (splitBy ' ' s) >> listen h
  where
    isPing x    = "PING :" `isPrefixOf` x
    doPong x    = write h "PONG" (':' : drop 6 x)

joinChan :: Handle -> [String] -> IO ()
joinChan h (x:xs)   = write h "JOIN" x >> joinChan h xs
joinChan _ []       = return ()

privmsg :: Handle -> String -> String -> IO ()
privmsg h c s = write h "PRIVMSG" (c ++ " :" ++ s)

notice :: Handle -> String -> String -> IO ()
notice h c s = write h "NOTICE" (c ++ " :" ++ s)

parseLine :: Handle -> [String] -> RandT (MarkovT IO) ()
parseLine h (prefix:command:params)
        | command == "001"      = liftIO ( write h "UMODE2" "+B" >>
                                           joinChan h autojoinChan )
        | command == "INVITE"   = liftIO ( joinChan h (tail params) )
        | command == "PRIVMSG"  = handleMsg h prefix params
        | otherwise             = return () -- Ignore
parseLine _ _                   = liftIO ( putStrLn "Couldn't parse line" )

handleMsg :: Handle -> String -> [String] -> RandT (MarkovT IO) ()
handleMsg h prefix (chan:(_:operation):args)
        | member op opList  = liftIO ( printf "%s called %s in %s\n"
                                     senderNick
                                     (op ++ foldl (++) "('" (intersperse "','" args) ++ "')")
                                     chan ) >>
                              hoist (hoist generalize) ((opList ! op) args) >>=
                              liftIO . ( notice h chan )
        | otherwise         = liftIO ( printf "%s said %s in %s\n"
                                       senderNick
                                       (foldl (++) "" . intersperse " " $ operation:args)
                                       chan ) >>
                              lift ( hoist generalize $ catalog senderNick (operation:args) )
        where
            op          = map toLower operation
            senderNick  = takeWhile (\c -> (c /='@') && (c /='!')) $ tail prefix
handleMsg _ _ _             = liftIO ( putStrLn "Couldn't handle message" )

-- Pure

catalog :: String -> [String] -> Markov ()
catalog user []      = return ()
catalog user words   = let
                         chainDiff = fromWalk stripMsg words
                         key = Name user
                         updateChain = insertWith markovAdd key chainDiff
                         updateAll = insertWith markovAdd All chainDiff
                       in
                         modify (updateChain . updateAll)
                   where
                       stripMsg :: String -> String
                       stripMsg s = s

markovAdd :: (Ord word) => MarkovChain word -> MarkovChain word -> MarkovChain word
markovAdd large small   = unionWith freqAdd large small

freqAdd :: (Ord word) => FrequencyMap word -> FrequencyMap word -> FrequencyMap word
freqAdd large small = unionWith (+) large small

fromWalk :: (Ord word) => (word -> word) -> [word] -> MarkovChain word
fromWalk _ []       = empty
fromWalk wrdfltr w  = mapWithKey (\k -> fromList) $ fromListWith (++) (transitionCount $ toTransitions wrdfltr $ w)

transitionCount :: (Ord word) => [[Maybe word]] -> [([Maybe word], [(Maybe word, Integer)])]
transitionCount transitions = map collate (group (sort transitions))
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

imitateall :: [String] -> RandT Markov String
imitateall seeds    = lift get >>= (\chains ->
                        hoist generalize . runImitate seeds $
                            findWithDefault empty All chains ) >>=
                      return . (\s -> colour 14 s)

imitate :: [String] -> RandT Markov String
imitate []  = return $ colour 4 "...must construct additional pylons..."
imitate (user:seeds)    = lift get >>= (\chains ->
                            hoist generalize . runImitate seeds $
                                case (readMaybe user :: Maybe Int) of
                                    Nothing ->
                                        let
                                          key = Name user
                                        in
                                          findWithDefault empty key chains
                                    Just i  ->
                                      if i >= 0 && i < size chains then
                                        snd $ elemAt i chains
                                      else
                                        let
                                          key = Name user
                                        in
                                          findWithDefault empty key chains) >>=
                          return . (\s -> "<"++user++"> "++(colour 10 s))

runImitate :: [String] -> MarkovChain String -> Rand String
runImitate seeds chain
        | chain == empty    = return "-"
        | otherwise         = startWalk chain 40 seeds >>=
                              return . joinMaybes
        where
            joinMaybes :: [Maybe String] -> String
            joinMaybes = foldl (++) "" . intersperse " " . catMaybes

startWalk :: (Ord word, Show word) => MarkovChain word -> Int -> [word] -> Rand [Maybe word]
startWalk chain _ _
        | chain == empty    = error "(startWalk) This shouldn't happen"
startWalk chain n []        = choose (keys chain) >>= \step ->
                                walk chain n step >>=
                                return . (++) step
startWalk chain n seeds     = let
                                keyList = keys chain
                                candidates = findMaxSetIntersectionList
                                                    (map Just seeds)
                                                    keyList
                              in
                                choose candidates >>= \step ->
                                    walk chain n step >>=
                                    return . (++) step

walk :: (Ord word, Show word) => MarkovChain word -> Int -> [Maybe word] -> Rand [Maybe word]
walk chain i last
        | i > 0     = step chain last >>= \next ->
                          trace ("Step from "++(show last)++" to "++(show next)) $
                          if next == Nothing then
                            return [Nothing]
                          else
                            walk chain (i-1) (tail last ++ [next]) >>= 
                            return . (next :)
        | i == 0    = return last

step :: (Ord word) => MarkovChain word -> [Maybe word] -> Rand (Maybe word)
step chain current = let
                        freqMap = findWithDefault (fromList [(Nothing, 1)]) current chain
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
                      return . search (toList choiceMap)
        where
            total = fold (+) 0 choiceMap
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
        | (i > 0) && (i < 10)   = (toEnum 3):'0':(show i) ++ s
        | (i > 0) && (i <= 15)  = (toEnum 3):(show i) ++ s

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

