{-# LANGUAGE ImplicitParams #-}

import System.IO
import Text.Regex.TDFA
import Network.Whois
import qualified Data.ByteString.Char8
import qualified Data.Text.Encoding
import qualified Data.Text
import Data.Maybe
import qualified System.Random as RND
import System.Console.ParseArgs
import qualified Control.Applicative as CA
import qualified Control.Concurrent as CC
import qualified Distribution.System as DS
import Data.Encoding.CP932
import Data.Encoding.UTF8
import qualified Data.Encoding
import qualified Codec.Text.IConv
import qualified System.IO.Encoding
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

getCompanyRegex :: String -> String
getCompanyRegex _ = "^f."

searchCompanyFromIpWithDelay :: String -> IO String
searchCompanyFromIpWithDelay ip = do
    delay <-  RND.getStdRandom(RND.randomR(1*1000^2,10*1000^2))
    CC.threadDelay delay
    searchCompanyFromIp ip

searchCompanyFromIp :: String -> IO String
searchCompanyFromIp ip = do
    contents <- whois ip
    if DS.buildOS == DS.Windows
        then do
            let sbs = Codec.Text.IConv.convert "UTF-8" "CP932" $ toL contents
            let sc = Data.Encoding.decodeLazyByteString CP932 sbs
            let c = (getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines) sc
            let ?enc = CP932
            System.IO.Encoding.putStrLn $ ip ++ "," ++ c
            return c
        else do
            let sc = Data.Encoding.decodeLazyByteString UTF8 $ toL contents
            let c = (getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines) sc
            let ?enc = UTF8
            System.IO.Encoding.putStrLn $ ip ++ "," ++ c
            return c

isCompany :: String -> String -> Bool
isCompany regex line = line =~ regex :: Bool

getCompanyName :: String -> String
getCompanyName line = line =~ "([^ ]*[ |.][^ ]+)$" :: String

makeCsv :: String -> String -> String
makeCsv a b = a ++ "," ++ b 

options :: [Arg String]
options =
    [ Arg "csv" (Just 'c') (Just "csv") (argDataOptional "FILE_PATH" ArgtypeString) "input csv file path"
    , Arg "out" (Just 'o') (Just "output") (argDataDefaulted "FILE_PATH" ArgtypeString "output.csv") "output file path"
    , Arg "ip" Nothing Nothing (argDataOptional "IP_ADDRESS" ArgtypeString) "ip address"
    ]

searchAndWrite :: (String -> IO String) -> Handle -> String -> IO ()
searchAndWrite func h ip = do
        companyName <- func ip
        hPutStrLn h $ ip ++ "," ++ companyName
        hFlush h

main :: IO ()
main = do
    a <- parseArgsIO ArgsComplete options
    output <- fromJust CA.<$> getArgFile a "out" WriteMode
    if gotArg a "csv"
        then do
            input <- getArgFile a "csv" ReadMode
            contents <- hGetContents $ fromJust input
            mapM_ (searchAndWrite searchCompanyFromIpWithDelay output) $ lines contents
            hClose output
        else if gotArg a "ip"
        then do
            searchAndWrite searchCompanyFromIp output $ fromJust $ getArgString a "ip"
            hClose output
        else do
            putStrLn $ argsUsage a
