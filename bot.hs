import Text.Printf (hPrintf, printf)
import Network (connectTo, PortID(PortNumber))
import Debug.Trace (trace, traceShow, traceShowId)

import Control.Applicative ()
import Control.Lens.Tuple (_1)
import Control.Lens ((&), (%~), over)

import System.Random (randomR, newStdGen, StdGen, Random)
import System.IO (Handle, hSetBuffering, hGetLine, BufferMode(..))

import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.List (isPrefixOf, elem, intersperse, sort, group, delete)
import Data.Map (Map, empty, member, findWithDefault, fromList, (!), 
                 unions, mapKeys, keys, insertWith, unionWith, toList,
                 mapWithKey, fromListWith, fold)

server          = "irc.sublumin.al"
port            = 6667
autojoinChan    = ["#spam"]
nick            = "mako"
markovOrder     = 2
opList          = unions (map (\f -> mapKeys f (fromList [("imitate", imitate), ("im", imitate)])) (map (:) "'"))

type FrequencyMap wordType = Map (Maybe wordType) Integer

type MarkovChain wordType = Map [Maybe wordType] (FrequencyMap wordType)

type ChainList keyType wordType = Map keyType (MarkovChain wordType)

data State = State (ChainList String String) StdGen

main    = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" "markovbot 0 * :Markov bot"
    rng <- newStdGen
    listen h (State empty rng)

write :: Handle -> String -> String -> IO ()
write h s t     = do
    hPrintf h "%s %s\r\n" s t
    printf     "=>> %s %s\n" s t

listen :: Handle -> State -> IO State
listen h state = do
    t <- hGetLine h
    let s = init t
    putStrLn s
    state <- if isPing s then doPong s else parseLine h state (splitBy ' ' s)
    listen h state
  where
    isPing x    = "PING :" `isPrefixOf` x
    doPong x    = write h "PONG" (':' : drop 6 x) >> return state

joinChan :: Handle -> [String] -> IO ()
joinChan h (x:xs)   = do
    write h "JOIN" x
    joinChan h xs
joinChan _ []       = return ()

privmsg :: Handle -> String -> String -> IO ()
privmsg h c s = write h "PRIVMSG" (c ++ " :" ++ s)

notice :: Handle -> String -> String -> IO ()
notice h c s = write h "NOTICE" (c ++ " :" ++ s)

parseLine :: Handle -> State -> [String] -> IO State
parseLine h state (prefix:command:params)
        | command == "001"              = do
                write h "UMODE2" "+B"
                joinChan h autojoinChan
                return state
        | command == "INVITE"           = joinChan h (tail params) >> return state
        | command == "PRIVMSG"          = handleMsg h state prefix params
        | otherwise                     = return state -- Ignore
parseLine _ state _                     = putStrLn "Couldn't parse line" >> return state

handleMsg :: Handle -> State -> String -> [String] -> IO State
handleMsg h state prefix (chan:(_:operation):args)
        | member op opList  = do
            putStrLn $ senderNick ++" called "++ op++(foldl (++) "('" (intersperse "','" args))++"')"++" in "++ chan
            let (newState, resp) = (opList ! op) state args
            notice h chan resp
            return newState
        | otherwise         = do
            putStrLn (senderNick ++" said "++ (foldl (++) "" (intersperse " " (operation:args)))++" in "++ chan)
            return $ catalog state senderNick (operation:args)
        where
            op          = map toLower operation
            senderNick  = takeWhile (\c -> (c /='@') && (c /='!')) $ tail prefix
handleMsg _ state _ _                             = putStrLn "Couldn't handle message" >> return state

-- Pure

catalog :: State -> String -> [String] -> State
catalog state user []               = state
catalog (State chains g) user words = let
                                        newChain = fromWalk strip words
                                      in State (insertWith markovAdd user newChain chains) g
                            where
                                strip :: String -> String
                                strip s = dropWhile (flip elem "<0123456789") $ filter (not . flip elem "\ETX>") s

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

imitate :: State -> [String] -> (State, String)
imitate state []                        = (state, colour 4 "...must construct additional pylons...")
imitate state@(State chains rng) (user:seeds)
                | targetChain == empty  = (state, "-")
                | otherwise = let
                                (markovWalk, newGen) = trace ("Trying imitate with chain: "++(show targetChain))
                                                        $ startWalk targetChain 40 seeds rng
                                imitation = foldl (++) "" $ intersperse " " (catMaybes markovWalk)
                              in (State chains newGen, "<"++user++"> "++(colour 10 imitation))
                where
                    targetChain = findWithDefault empty user chains

startWalk :: (Ord word, Show word) => MarkovChain word -> Int -> [word] -> StdGen -> ([Maybe word], StdGen)
startWalk chain _ _ _ | chain == empty  = error "(startWalk) This shouldn't happen"
startWalk chain n [] rng                = let 
                                            (firstStep, newGen) = choose (keys chain) rng
                                          in over _1 (firstStep++) $ walk chain n firstStep newGen
startWalk chain n seeds rng             = let
                                            keyList = keys chain
                                            candidates = findMaxSetIntersectionList (map Just seeds) keyList
                                            (seedStep, newGen) = choose candidates rng
                                          in over _1 (seedStep++) $ walk chain n seedStep newGen

walk :: (Ord word, Show word) => MarkovChain word -> Int -> [Maybe word] -> StdGen -> ([Maybe word], StdGen)
walk chain i last rng
        | i > 0     = let
                        (next, newGen) = step chain last rng
                        (rest, finalGen) = walk chain (i-1) (tail last ++ [next]) newGen
                      in trace ("Step from "++(show last)++" to "++(show next)) $
                          if next == Nothing then
                            ([Nothing], newGen)
                          else
                            (next : rest, finalGen)
        | i == 0    = (last, rng)

step :: (Ord word) => MarkovChain word -> [Maybe word] -> StdGen -> (Maybe word, StdGen)
step chain current rng = let freqMap = findWithDefault (fromList [(Nothing, 1)]) current chain
                         in sample freqMap rng 

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

sample :: FrequencyMap word -> StdGen -> (Maybe word, StdGen)
sample choiceMap rng        = (search (toList choiceMap) randNum, newGen)
        where
            total = fold (+) 0 choiceMap
            (randNum, newGen) = randomR (0, total-1) rng
            search mapList val = case mapList of
                            ((a,b):xs)  -> if val >= b then search xs (val - b) else a
                            []          -> error "(sample) This shouldn't happen"

choose :: [a] -> StdGen -> (a, StdGen)
choose [] rng               = error "(choose) This shouldn't happen"
choose choices rng          = (choices !! index, newGen)
        where
            (index, newGen) = randomR (0, length choices - 1) rng

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

