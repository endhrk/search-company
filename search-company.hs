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
    let c = getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines $ Data.Text.unpack $ Data.Text.Encoding.decodeUtf8 contents
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
        Data.ByteString.Char8.hPutStrLn h $ Data.Text.Encoding.encodeUtf8 $ Data.Text.pack $ makeCsv ip companyName
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
