module IrcMessage (
    module IrcMessage,
    module Control.Monad.Writer.Lazy
) where

import Config

import Prelude hiding ()

import qualified Text.Printf as T (hPrintf)

import qualified Data.Text as T (Text(..), pack, unpack, toLower, isPrefixOf, drop, words, tail, singleton, break)
import qualified Data.Text.IO as T (putStrLn, hPutStrLn, hGetLine)

import System.IO (Handle)

import Control.Monad.Writer.Lazy (Writer(..), execWriter, tell)

import qualified Data.List as List (intercalate, map, concatMap)
import qualified Data.Map as Map (Map(..), lookup)

import Debug.Trace

type Map = Map.Map
type Text = T.Text

data Prefix = Server { name :: Text } | User { nick :: Text, real :: Text, host :: Text } deriving (Eq)

instance Show Prefix where
    showsPrec x (Server name)         = showString (T.unpack name)
    showsPrec x (User nick real host) = showString (T.unpack nick) . showChar '!' . 
                                        showString (T.unpack real) .  showChar '@' . 
                                        showString (T.unpack host)

instance Read Prefix where
    readsPrec x pfx = case span (/='@') pfx of
                        (name, "") ->
                          return (Server $ T.pack name, "")
                        (names, '@':host) ->
                          let
                            (nick, '!':real) = span (/='!') names
                          in
                            return (User (T.pack nick) (T.pack real) (T.pack host), "")

data Code = RPL_Welcome
          | ERR_NicknameInUse
          deriving (Eq)

instance Show Code where
    showsPrec x RPL_Welcome         = showString "001"
    showsPrec x ERR_NicknameInUse   = showString "433"

data Command = Numeric Code | Register | Nick | Pass | Ping | Pong | Umode2 | Join | Invite | Privmsg | Notice | Undefined String deriving (Eq)

instance Show Command where
    showsPrec x (Numeric rpl) = showsPrec x rpl
    showsPrec x Register      = showString "USER"
    showsPrec x Nick          = showString "NICK"
    showsPrec x Pass          = showString "PASS"
    showsPrec x Ping          = showString "PING"
    showsPrec x Pong          = showString "PONG"
    showsPrec x Umode2        = showString "UMODE2"
    showsPrec x Join          = showString "JOIN"
    showsPrec x Invite        = showString "INVITE"
    showsPrec x Privmsg       = showString "PRIVMSG"
    showsPrec x Notice        = showString "NOTICE"
    showsPrec x (Undefined t) = showString t

instance Read Command where
    readsPrec x "001"     = return (Numeric RPL_Welcome, "")
    readsPrec x "433"     = return (Numeric ERR_NicknameInUse, "")
    readsPrec x "USER"    = return (Register, "")
    readsPrec x "NICK"    = return (Nick, "")
    readsPrec x "PASS"    = return (Pass, "")
    readsPrec x "PING"    = return (Ping, "")
    readsPrec x "PONG"    = return (Pong, "")
    readsPrec x "UMODE2"  = return (Umode2, "")
    readsPrec x "JOIN"    = return (Join, "")
    readsPrec x "INVITE"  = return (Invite, "")
    readsPrec x "PRIVMSG" = return (Privmsg, "")
    readsPrec x "NOTICE"  = return (Notice, "")
    readsPrec x s         = return (Undefined s, "")

data Params = Short { param :: Text, next :: Params }
            | Long { param :: Text }
            | End deriving (Eq)

instance Show Params where
    showsPrec x (Short param next)  = (showString $ T.unpack param) .
                                      case next of
                                        End ->
                                          id
                                        param ->
                                          showChar ' ' . showsPrec x next
    showsPrec x (Long param)        = showChar ':' . (showString $ T.unpack param)
    showsPrec x (End)               = id

instance Read Params where
    readsPrec x ""          = return (End, "")
    readsPrec x (':':param) = return (Long $ T.pack param, "")
    -- TODO: Unshittify this
    readsPrec x params      = let
                                (param, rest) = break (==' ') . dropWhile (==' ') $ params
                              in
                                return (Short (T.pack param) (read . drop 1 $ rest), "")

data Message = Message {
    prefix  :: Maybe Prefix,
    command :: Command,
    params  :: Params
} deriving (Eq)

instance Show Message where
    showsPrec x msg = showPrefix . showCommand . showParams
                    where
                      showPrefix = case prefix msg of
                                      Nothing ->
                                        id
                                      Just prefix ->
                                        showChar ':' . showsPrec x prefix . showChar ' '
                      showCommand = showsPrec x (command msg)
                      showParams = case params msg of
                                      End ->
                                        id
                                      param ->
                                        showChar ' ' . showsPrec x param

instance Read Message where
    readsPrec x (':':line)  = let 
                                (pfx, msg') = break (==' ') line
                                msg = dropWhile (==' ') msg'
                                (cmd, params') = break (==' ') msg
                                params = dropWhile (==' ') params'
                              in
                                return (Message (Just $ read pfx) (read cmd) (read params), "")
    readsPrec x msg         = let
                                (cmd, params') = break (==' ') msg
                                params = dropWhile (==' ') params'
                              in
                                return (Message Nothing (read cmd) (read params), "")

irc_user :: (Writeable a, Writeable b) => a -> b -> Message
irc_user ident realname = Message Nothing Register (Short (text ident)
                                                  $ Short (T.singleton '-')
                                                  $ Short (T.singleton '*')
                                                  $ Long (text realname))

irc_nick :: Writeable a => a -> Message
irc_nick nick = Message Nothing Nick ( Short (text nick) End )

irc_pass :: Writeable a => a -> Message
irc_pass pass = Message Nothing Pass ( Short (text pass) End )

irc_umode2 :: Writeable a => a -> Message
irc_umode2 mode = Message Nothing Umode2 ( Short (text mode) End )

irc_join :: Writeable a => a -> Message
irc_join chan = Message Nothing Join ( Short (text chan) End )

irc_privmsg :: (Writeable a, Writeable b) => a -> b -> Message
irc_privmsg chan msg = Message Nothing Privmsg ( Short (text chan) ( Long (text msg) ) )

irc_pong :: Writeable a => a -> Message
irc_pong code = Message Nothing Pong ( Long (text code) )

parseLine :: Message -> Writer [Message] ()
parseLine msg = case msg of
                  Message _ (Numeric RPL_Welcome) _ ->
                    tell [irc_umode2 "+B"] >>
                    mapM_ (tell . pure . irc_join) autoJoin
                  Message Nothing Ping (Long code) ->
                    tell [irc_pong . T.unpack $ code]
                  Message _ Invite (Short nick (Short chan End)) ->
                    tell [irc_join . T.unpack . param . next $ params msg]
                  Message (Just pfx) Privmsg (Short dest (Long content)) ->
                    handleMsg pfx dest content
                  msg ->
                    return () -- Ignore
        where
            autoJoin :: [String]
            autoJoin = []

handleMsg :: Prefix -> Text -> Text -> Writer [Message] ()
handleMsg pfx dest msg = let
                           (cmd, args) = T.break (==' ') msg
                         in
                           case Map.lookup (T.unpack cmd) opList of
                             Just operation ->
                               return ()
                             Nothing ->
                               return ()

class WriteableS a where
    textS :: [a] -> Text

instance WriteableS Char where 
    textS msg = T.pack msg

instance WriteableS a => Writeable [a] where
    text = textS

class Writeable a where
    text :: a -> Text

instance Writeable Message where
    text msg = T.pack $ show msg

instance Writeable T.Text where
    text msg = msg

