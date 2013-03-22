import System.IO
import System.Environment
import Text.Regex.TDFA
import Control.Exception as CE
import Network.Whois
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding
import qualified Data.Text
import Data.Maybe
import System.Console.ParseArgs
import qualified Control.Applicative as CA

getCompanyRegex :: String -> String
getCompanyRegex _ = "^f."

searchCompanyFromIp :: String -> IO String
searchCompanyFromIp ip = do
    contents <- whois ip
    return . getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines $ Data.Text.unpack $ Data.Text.Encoding.decodeUtf8 contents

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

main :: IO ()
main = do
    a <- parseArgsIO ArgsComplete options
    output <- fromJust CA.<$> getArgFile a "out" WriteMode
    if gotArg a "csv"
        then do
            input <- getArgFile a "csv" ReadMode
            contents <- hGetContents $ fromJust input
            companies <- mapM searchCompanyFromIp $ lines contents
            mapM_ (hPutStrLn output) $ zipWith makeCsv (lines contents) companies
            hClose output
        else if gotArg a "ip"
        then do
            let ip = fromJust $ getArgString a "ip"
            companyName <- searchCompanyFromIp ip
            hPutStrLn output (makeCsv ip companyName)
            hClose output
        else do
            putStrLn $ argsUsage a
