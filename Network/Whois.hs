module Network.Whois
    ( whois
    ) where

import Network
import System.IO
import System.Environment (getArgs)
import Codec.Text.IConv (convert)
import Data.ByteString.Lazy.Char8 as BS (pack,unpack,hGetContents,ByteString)
import Codec.Binary.UTF8.String (encodeString,decodeString)
import System.Info as SYS (os)
import Control.Applicative

convertCode :: String -> ByteString -> String
convertCode code src = decodeString $ unpack $ convert code (getOSCharCode) src

getOSCharCode :: String
getOSCharCode = if (SYS.os == "mingw32") then "Shift-JIS" else "UTF-8"

getWhoisServer :: String -> String
getWhoisServer _ = "192.41.192.40"

getWhoisCharCode :: String -> String
getWhoisCharCode _ = "ISO-2022-JP"

whois :: String -> IO String
whois ip = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    h <- connectTo (getWhoisServer ip) (PortNumber 43)
    hSetBuffering h LineBuffering
    hPutStrLn h ip
    convertCode (getWhoisCharCode ip) <$> BS.hGetContents h

usage :: IO ()
usage = putStrLn "usage: whois [IP_ADDR]"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then usage
        else do
          (whois $ head args) >>= putStrLn
