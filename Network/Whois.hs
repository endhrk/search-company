{-# LANGUAGE ScopedTypeVariables #-}

module Network.Whois
    ( whois
    ) where

import Network
import System.IO
import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString as BS
import Control.Exception
import Data.IP

toS :: BSL.ByteString -> BS.ByteString
toS = BS.concat . BSL.toChunks

makeIpAddr :: String -> IPv4
makeIpAddr ip = read ip :: IPv4

makeIpAddrRange :: String -> AddrRange IPv4
makeIpAddrRange ip = read ip :: AddrRange IPv4

getJpnicList :: String -> IO [String]
getJpnicList path = do contents <- readFile path
                       return $ lines contents


getWhoisServer :: String -> IO String
getWhoisServer ip = do iplist <- getJpnicList "Network/jpnic_list"
                       if (or $ map (isMatchedTo (makeIpAddr ip)) (map makeIpAddrRange iplist))
                           then return "whois.nic.ad.jp"
                           else return "whois.apnic.net"

getWhoisCharCode :: String -> IO String
getWhoisCharCode ip = do iplist <- getJpnicList "Network/jpnic_list"
                         if (or $ map (isMatchedTo (makeIpAddr ip)) (map makeIpAddrRange iplist))
                             then return "ISO-2022-JP"
                             else return "UTF-8"


whois :: String -> IO BS.ByteString
whois ip = do
    Control.Exception.catch
        (withSocketsDo $ do
            hSetBuffering stdout NoBuffering
            host <- getWhoisServer ip
            h <- connectTo host (PortNumber 43)
            hSetBuffering h LineBuffering
            hPutStrLn h ip
            c <- BSLC.hGetContents h
            charset <- getWhoisCharCode ip
            return $ toS $ convert charset "UTF-8" c
        )
        (\(_ :: SomeException) -> do
            host <- getWhoisServer ip
            hPrint stderr ("connection failed to whois server\"" ++ host ++ "\".")
            return BS.empty
        )

main :: IO()
main = do
    result <- whois "221.249.116.206"
    putStrLn $ show result
    result <- whois "220.104.10.20"
    putStrLn $ show result
