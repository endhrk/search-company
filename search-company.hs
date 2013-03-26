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

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

getCompanyRegex :: String -> String
getCompanyRegex _ = "^f."

isCompany :: String -> String -> Bool
isCompany regex line = line =~ regex :: Bool

matchCompanyName :: String -> String
matchCompanyName line = line =~ "([^ ]*[ |.][^ ]+)$" :: String

delay :: IO ()
delay = do
    delayTime <-  RND.getStdRandom(RND.randomR(1*1000*1000,10*1000*1000))
    threadDelay delayTime

getCompanyName :: String -> IO String
getCompanyName ip = do
    w <- whois ip
    return $ getName $ toUTF w
    where
        toUTF bs = decodeLazyByteString UTF8 $ toL bs
        getName s = (matchCompanyName . unlines . filter (isCompany (getCompanyRegex ip)) . lines) s

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
    System.IO.Encoding.hPutStrLn output $ ip ++ "," ++ cs
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
