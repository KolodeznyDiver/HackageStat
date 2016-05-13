module Main where

import System.IO
import Network.HTTP
import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB
import Text.Parsec
import Data.Char(toLower,toUpper)
import Control.Monad

type ParserFunType = Parsec String ()

caseInsensitiveChar:: Char -> ParserFunType ()
caseInsensitiveChar c = void $ char (toLower c) <|> char (toUpper c)

caseInsensitiveString:: String -> ParserFunType ()
caseInsensitiveString s = try (mapM_ caseInsensitiveChar s)

download:: Manager -> String -> IO ()
download m n = do 
    req <- parseUrl ("http://hackage.haskell.org/package/"++n)
    withHTTP req m $ \resp ->
        withBinaryFile (concat["package/",n,".html"]) WriteMode $ \h ->
                runEffect $ responseBody resp >-> PB.toHandle h

main :: IO ()
main = do
    m <- newManager defaultManagerSettings
    a <- simpleHTTP (getRequest "http://hackage.haskell.org/packages/top")
    case a of
        (Left err) -> print err
        (Right resp) -> let body = rspBody resp in
                        case parse (parsePkgNames []) "" body of
                            Left perr  -> print perr
                            Right xs  -> 
                                let sLn = concat [" from ", show $ length xs,
                                            " : "] in
                                forM_ (zip xs [1 :: Int ..]) $ \(n,i) -> do
                                        putStrLn $ concat [ "downloading ",
                                            show i,sLn,n]
                                        download m n 
                                          
  where parsePkgNames l = do
            skipMany $ noneOf "<"
            r <- optionMaybe lnk
            case r of 
                Just n -> parsePkgNames $ n:l
                _ -> do
                    e <- optionMaybe anyToken
                    case e of
                      Nothing -> return l
                      _ -> parsePkgNames l
        lnk = do
            caseInsensitiveString "<a href=\"/package/"
            many1 (noneOf "\"")
