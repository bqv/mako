module Logging (
    Entry(..),
    write,
    err,
    warn,
    info,
    debug
) where

data LogLevel = Error | Warn | Info | Debug

data Entry = Log LogLevel String

write :: Entry -> IO ()
write (Log lvl str) = putStrLn str

err s = Log Error s
warn s = Log Warn s
info s = Log Info s
debug s = Log Debug s
