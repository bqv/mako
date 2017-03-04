module Markov (
    imitate,
    catalog
) where

import Prelude hiding (words, unwords, head, lookup)

import Control.Monad (guard)
import Control.Monad.Writer.Lazy (WriterT(..), execWriterT, tell)
import Control.Monad.State.Strict (StateT, State, MonadState, get, put, modify, gets,
                            state, liftIO, lift, evalStateT, evalState)
import Control.Comonad (liftW, extend, (=>>))

import System.Random (StdGen, Random, RandomGen, randomR, newStdGen)

import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)
import Data.Text (Text(..), pack, unpack, words, unwords)
import Data.Hashable (Hashable(..), hashWithSalt)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, head, toList)
import qualified Data.HashMap.Strict as H (HashMap(..), insertWith, union, unionWith, empty,
                                           fromListWith, singleton, lookup, keys, toList)

type MarkovData = H.HashMap Text MarkovChainArray -- Name -> Chain
type MarkovChainArray = (MarkovChain Unigram, MarkovChain Bigram, MarkovChain Trigram)
type MarkovChain a = H.HashMap a MarkovEntry -- NGram -> Assocs
type MarkovEntry = H.HashMap Text Int -- Candidate -> Value

data Unigram = Unigram !Text deriving (Eq, Show)
data Bigram = Bigram !Text !Text deriving (Eq, Show)
data Trigram = Trigram !Text !Text !Text deriving (Eq, Show)

instance Hashable Unigram where
    hashWithSalt s (Unigram a)      = s `hashWithSalt` a
instance Hashable Bigram where
    hashWithSalt s (Bigram a b)     = s `hashWithSalt`
                                      a `hashWithSalt` b
instance Hashable Trigram where
    hashWithSalt s (Trigram a b c)  = s `hashWithSalt`
                                      a `hashWithSalt`
                                      b `hashWithSalt` c

data NGram = TheUnigram Unigram
           | TheBigram Bigram
           | TheTrigram Trigram
           deriving (Eq, Show)

imitate :: [Text] -> Text -> StateT MarkovData (StateT StdGen (Maybe)) Text
imitate seed name = let
                        (t,h) = bimap reverse reverse . splitAt 3 $ reverse seed
                        ng = toNGram t
                    in
                        get >>= 
                        lift . lift . H.lookup name >>=
                        lift . runMarkov ng >>=
                        return . unwords . (seed++) . (:[])
        where
            toNGram :: [Text] -> Maybe NGram
            toNGram [] = Nothing
            toNGram [a] = Just (TheUnigram $ Unigram a)
            toNGram [a,b] = Just (TheBigram $ Bigram a b)
            toNGram [a,b,c] = Just (TheTrigram $ Trigram a b c)

runMarkov :: Maybe NGram -> MarkovChainArray -> StateT StdGen (Maybe) Text
runMarkov Nothing (c1,c2,c3) = choose [1,2,3] >>= 
                               getNGram >>=
                               flip runMarkov (c1,c2,c3) . Just
        where
            getNGram :: Int -> StateT StdGen Maybe NGram
            getNGram 1 = choose (H.keys c1) >>= return . TheUnigram
            getNGram 2 = choose (H.keys c2) >>= return . TheBigram
            getNGram 3 = choose (H.keys c3) >>= return . TheTrigram
runMarkov (Just ng) (c1,c2,c3) = execWriterT (markovWalk (c1,c2,c3) ng) >>= \words ->
                                     case words of
                                     [] -> lift Nothing
                                     words -> return $ unwords words

markovWalk :: MarkovChainArray -> NGram -> WriterT [Text] (StateT StdGen (Maybe)) ()
markovWalk (c1,c2,c3) ng = case ng of
                             TheUnigram ug@(Unigram a) ->
                                (lift . lift) (H.lookup ug c1) >>=
                                lift . weightedChoose >>= \t ->
                                    tell (pure t) >>
                                    markovWalk (c1,c2,c3) (TheUnigram (Unigram t))
                             TheBigram bg@(Bigram a b) ->
                                (lift . lift) (H.lookup bg c2) >>=
                                lift . weightedChoose >>= \t ->
                                    tell (pure t) >>
                                    markovWalk (c1,c2,c3) (TheBigram (Bigram b t))
                             TheTrigram tg@(Trigram a b c) ->
                                (lift . lift) (H.lookup tg c3) >>=
                                lift . weightedChoose >>= \t ->
                                    tell (pure t) >>
                                    markovWalk (c1,c2,c3) (TheTrigram (Trigram b c t))
        where
            weightedChoose :: MarkovEntry -> StateT StdGen Maybe Text
            weightedChoose hmap  = choose . concat . map (\(k,v) -> replicate v k) $ H.toList hmap

choose :: [a] -> StateT StdGen Maybe a
choose []       = lift Nothing
choose choices  = (state . randomR) (0, length choices - 1) >>=
                  return . (choices !!)

catalog :: Text -> Text -> State MarkovData ()
catalog name text = modify . H.insertWith mergeAllChains name . genChain . getWords $ text
        where
            mergeAllChains :: MarkovChainArray -> MarkovChainArray -> MarkovChainArray
            mergeAllChains (u1,b1,t1) (u2,b2,t2) = (mergeChains u1 u2, mergeChains b1 b2, mergeChains t1 t2)
            getWords :: Text -> Maybe (NonEmpty Text)
            getWords = nonEmpty . words

mergeChains :: (Eq a, Hashable a) => MarkovChain a -> MarkovChain a -> MarkovChain a
mergeChains a b = H.unionWith mergeEntries a b

mergeEntries :: MarkovEntry -> MarkovEntry -> MarkovEntry
mergeEntries a b = H.unionWith (+) a b

genChain :: Maybe (NonEmpty Text) -> MarkovChainArray
genChain Nothing = (H.empty, H.empty, H.empty)
genChain (Just wds) = (genUnigrams, genBigrams, genTrigrams)
        where
            singletonUnigram :: [Text] -> MarkovChain Unigram
            singletonUnigram [a,b] = H.singleton (Unigram a) (H.singleton b 1)
            genUnigrams :: MarkovChain Unigram
            genUnigrams = foldl mergeChains H.empty . fmap singletonUnigram . catMaybes . toList $ wds =>>
                                                                                                   takeMaybe 2
            singletonBigram :: [Text] -> MarkovChain Bigram
            singletonBigram [a,b,c] = H.singleton (Bigram a b) (H.singleton c 1)
            genBigrams :: MarkovChain Bigram
            genBigrams = foldl mergeChains H.empty . fmap singletonBigram . catMaybes . toList $ wds =>>
                                                                                                  takeMaybe 3
            singletonTrigram :: [Text] -> MarkovChain Trigram
            singletonTrigram [a,b,c,d] = H.singleton (Trigram a b c) (H.singleton d 1)
            genTrigrams :: MarkovChain Trigram
            genTrigrams = foldl mergeChains H.empty . fmap singletonTrigram . catMaybes . toList $ wds =>>
                                                                                                   takeMaybe 4

takeMaybe :: Int -> NonEmpty a -> Maybe [a]
takeMaybe n l = go n (Just l) []
        where
            go 0 _ acc = Just acc
            go x Nothing acc = Nothing
            go x (Just (a :| b)) acc = go (x-1) (nonEmpty b) (a:acc)

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

