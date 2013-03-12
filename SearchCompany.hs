import System.IO
import System.Environment
import Text.Regex.TDFA
import Control.Exception as CE
import Network.Whois

getCompanyRegex :: String -> String
getCompanyRegex _ = "^f."

searchCompanyFromIp :: String -> IO String
searchCompanyFromIp ip = do
    contents <- whois ip
    return . getCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines $ contents

isCompany :: String -> String -> Bool
isCompany regex line = line =~ regex :: Bool

getCompanyName :: String -> String
getCompanyName line = line =~ "([^ ]*[ |.][^ ]+)$" :: String

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
dispatch _ _ = do usage

catchException :: String -> IOError -> IO String
catchException filename _ = do
    hPutStrLn stderr $ "File not found: " ++ filename
    return []

usage :: IO ()
usage = putStrLn "usage: searchCompany [--ip IP_ADDR|--csv FILE_NAME]"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then usage
        else do
             (command:ip:etc) <- return args
             dispatch command ip
