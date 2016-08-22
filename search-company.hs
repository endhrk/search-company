{-# LANGUAGE ImplicitParams #-}

import System.IO
import Text.Regex.TDFA
import Network.Whois
import Data.Maybe
import qualified System.Random as RND
import System.Console.ParseArgs
import qualified Control.Applicative as CA
import Control.Concurrent (threadDelay)
import Distribution.System (buildOS, OS(..))
import Data.Encoding.CP932
import Data.Encoding.UTF8
import Data.Encoding
import Codec.Text.IConv (convert)
import qualified System.IO.Encoding
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Control.Monad (forM_)
import Data.String.Utils

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

getJpnicCompanyRegex :: String -> String
getJpnicCompanyRegex _ = "^f."

getApnicCompanyRegex :: String -> String
getApnicCompanyRegex _ = "^descr:"

isCompany :: String -> String -> Bool
isCompany regex line = line =~ regex :: Bool

get3rd (_,_,c) = c

matchCompanyName :: String -> String
matchCompanyName line = get3rd $ (line =~ "(  +)" :: (String,String,String))

filterApnic :: String -> String
filterApnic name = if (name =~ "APNIC" :: Bool) then "" else name

isJpnic :: String -> Bool
isJpnic str = not (str =~ "whois.apnic.net" :: Bool)

delay :: IO ()
delay = do
    delayTime <-  RND.getStdRandom(RND.randomR(5*1000*1000,15*1000*1000))
    threadDelay delayTime

getCompanyName :: String -> IO String
getCompanyName ip = do
    bs <- whois ip
    let str = toUTF bs
    if isJpnic $ head $ lines str
        then
            return $ getJpnicName str
        else
            return $ filterApnic $ getApnicName str
    where
        toUTF bs = decodeLazyByteString UTF8 $ toL bs
        getJpnicName s = (matchCompanyName . head . getList . filter (isCompany (getJpnicCompanyRegex ip)) . lines) s
        getApnicName s = (matchCompanyName . head . getList . filter (isCompany (getApnicCompanyRegex ip)) . lines) (last $ split "% Information related" s)
        getList list = if null list then [""] else list

options :: [Arg String]
options =
    [ Arg "csv" (Just 'c') (Just "csv")
      (argDataOptional "FILE_PATH" ArgtypeString) "input csv file path"
    , Arg "out" (Just 'o') (Just "output")
      (argDataDefaulted "FILE_PATH" ArgtypeString "output.csv") "output file path"
    , Arg "ip" Nothing Nothing
      (argDataOptional "IP_ADDRESS" ArgtypeString) "ip address"
    ]

putCP932 :: Handle -> String -> String -> IO ()
putCP932 output ip name = do
    let cs = (decodeLazyByteString CP932 . convert "UTF-8" "CP932" . encodeLazyByteString UTF8) name
    let ?enc = CP932
    System.IO.Encoding.hPutStr output $ ip ++ "," ++ cs ++ "\r\n"
    System.IO.Encoding.putStrLn $ ip ++ "," ++ cs
    hFlush output

putUTF8 :: Handle -> String -> String -> IO ()
putUTF8 output ip name = do
    let ?enc = UTF8
    System.IO.Encoding.hPutStrLn output $ ip ++ "," ++ name
    System.IO.Encoding.putStrLn $ ip ++ "," ++ name
    hFlush output

main :: IO ()
main = do
    a <- parseArgsIO ArgsComplete options
    output <- fromJust CA.<$> getArgFile a "out" WriteMode
    if gotArg a "csv"
        then do
            h <- getArgFile a "csv" ReadMode
            list <- hGetContents $ fromJust h
            forM_ (lines list) $ \ip -> do
                name <- getCompanyName ip
                putWithEncode ip name output
                delay
            hClose output
        else if gotArg a "ip"
        then do
            let ip = fromJust $ getArgString a "ip"
            name <- getCompanyName ip
            putWithEncode ip name output
            hClose output
        else do
            putStrLn $ argsUsage a
    where
        putWithEncode ip name output = if buildOS == Windows then putCP932 output ip name else putUTF8 output ip name
