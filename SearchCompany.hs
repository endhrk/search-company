import Network
import System.IO
import System.Environment
import Codec.Text.IConv (convert)
import Data.ByteString.Lazy.Char8 (pack,unpack)
import Codec.Binary.UTF8.String (encodeString,decodeString)
import Control.Exception (evaluate)
import Text.Regex.TDFA
import Control.Exception as CE

convertCode :: String -> String -> String
convertCode code src = decodeString $ unpack $ convert code "UTF-8" $ pack $ encodeString src

getWhoisServer :: String -> String
getWhoisServer ip = "192.41.192.40"

getWhoisCharCode :: String -> String
getWhoisCharCode ip = "ISO-2022-JP"

getCompanyRegex :: String -> String
getCompanyRegex ip = "^f."

searchCompanyFromIp :: String -> IO String
searchCompanyFromIp ip = do
    contents <- whois ip
    return . getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines $ contents

isCompany :: String -> String -> Bool
isCompany regex line = line =~ regex :: Bool

getCompanyName :: String -> String
getCompanyName line = line =~ "([^ ]*[ |.][^ ]+)$" :: String


whois :: String -> IO String
whois ip = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    h <- connectTo (getWhoisServer ip) (PortNumber 43)
    hSetBuffering h LineBuffering
    hPutStrLn h ip
    contents <- (hGetContents h >>= evaluate)
    hClose h
    return $ convertCode (getWhoisCharCode ip) contents

makeCsv :: String -> String -> String
makeCsv a b = a ++ "," ++ b 

dispatch :: String -> String -> IO ()
dispatch "--ip" ip = do
    companyName <- searchCompanyFromIp ip
    putStrLn $ makeCsv ip companyName
dispatch "--csv" fileName = do
    cs <- CE.catch (readFile fileName) (catchException fileName)
    companies <- mapM searchCompanyFromIp $ lines cs
    mapM_ putStrLn $ zipWith makeCsv (lines cs) companies

catchException :: String -> IOError -> IO String
catchException filename error = do
    hPutStrLn stderr $ "File not found: " ++ filename
    return []

usage = putStrLn "usage: searchCompany [--ip IP_ADDR|--csv FILE_NAME]"

main = do
    args <- getArgs
    if length args /= 2
        then usage
        else do
             (command:ip:etc) <- return args
             dispatch command ip
