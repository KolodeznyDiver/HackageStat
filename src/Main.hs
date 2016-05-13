{-# LANGUAGE OverloadedStrings,PatternSynonyms,ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Simple
import qualified Data.Text as T
import Data.Text.Read
import Text.XML
import Control.Monad (forM_)
import Text.XML.Lens
import System.EasyFile
import Options.Applicative

pattern SiteURL = ("http://hackage.haskell.org") :: String
pattern TopPage = ("/packages/top")  :: String
pattern PackagesPrefix = ("/package/")  :: String
pattern LocalOutDir = ("package")  :: String

data Cfg = Cfg { dwnldPerMonthLimit :: Int
               }
            
cfgParser:: Parser Cfg
cfgParser = Cfg <$>
    option auto (
        value 0 <>
        long "downloadsLimit" <>
        short 'd' <>
        metavar "Number" <>
        help "Minimal downloads per month for load" )

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> cfgParser)
      ( -- fullDesc <>
      progDesc "Download html package desciptions from hackage.haskell.org"
      <> header "HackageStat v0.1.0.1- Hackage package description downloader" )

main' :: Cfg -> IO ()
main' (Cfg dwnldsLim) = do
    let topPg = SiteURL ++ TopPage
    putStrLn $ concat ["Downloading ", topPg, " ..."]
    m <- newManager defaultManagerSettings
    request <- parseRequest $ topPg
    response <- httpLBS $ setRequestManager m request
    putStrLn $ concat ["Parsing ", topPg, " ..."]
    case parseLBS def{psDecodeEntities = decodeHtmlEntities} $ getResponseBody response of
        Left uerr  -> print uerr
        Right doc -> do
            let hel n = el Name{nameLocalName = n, 
                           nameNamespace = Just "http://www.w3.org/1999/xhtml",
                           namePrefix = Nothing}
                rows = doc ^.. root ./ hel "body" ./ attributeIs "id" "content"
                        ./ entire ./ hel "tr" 
                byDwnlds i [] = i
                byDwnlds i (s:ss) = case decimal s of
                                        Right (d,_) 
                                            | d < dwnldsLim -> i
                                            | otherwise -> byDwnlds (i+1) ss
                                        _ -> i
                dwnlds = rows ^.. traverse ./ hel "td" . text
                allPkgs = rows ^.. traverse ./ hel "td" ./ hel "a" . text
                pkgs = map T.unpack $ take (byDwnlds 0 dwnlds) allPkgs
                cAllPk = show $ length allPkgs
                cPk = show $ length pkgs
            putStrLn $ concat ["A total of ", cAllPk, 
               " packets of them fulfill the criteria ",cPk]
            let sLn = concat [" from ", cPk," : "] 
            createDirectoryIfMissing False outDir
            forM_ (zip pkgs [1 :: Int ..]) $ \(n,i) -> do
                putStrLn $ concat [ "downloading ",show i,sLn,n]
                download m n 
            putStrLn "Loading completed"
  where download m n = do
            request <- parseRequest $ concat[SiteURL,PackagesPrefix,n]
            response <- httpLBS $ setRequestManager m request
            L8.writeFile (outDir </> n System.EasyFile.<.> "html") $
                getResponseBody response
        outDir = normalise $ joinPath [".",LocalOutDir]
