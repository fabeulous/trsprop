module Main where

import Control.Monad
import Data.Rewriting.Problem hiding (map)
import Data.Rewriting.Rule (Rule)
import System.Directory (withCurrentDirectory)
import System.Environment
import System.Exit
import System.IO

import Properties

data Input = StdIn | File FilePath
    deriving Show

data Config = Config { input :: Input
                     , tests :: [[Rule String String] -> Bool]
                     , dir :: FilePath
                     , help :: Bool
                     }

defaultConfig :: Config
defaultConfig = Config { input = StdIn
                       , tests = []
                       , dir = "./"
                       , help = False
                       }

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ usage stderr >> exitFailure
    hSetBuffering stdout LineBuffering
    case parseArgs args of
      Left err -> hPutStrLn stderr err >> exitFailure
      Right cfg -> do
          when (help cfg) $ usage stdout >> exitSuccess
          run cfg

run :: Config -> IO ()
run cfg = do
    inp <- case input cfg of
             StdIn -> return stdin
             File file -> openFile file ReadMode
    trsFiles <- lines <$> hGetContents inp
    withCurrentDirectory (dir cfg) $
        forM_ trsFiles $ \file -> do
            let test x = all ($ x) $ tests cfg
            res <- fileFilter test file
            when res $ putStrLn file
    hClose inp

parseArgs :: [String] -> Either String Config
parseArgs = flip go defaultConfig
    where
        go [] cfg = return cfg
        go ("":args) cfg = go args cfg
        go ("-h":args) cfg = return cfg { help = True }
        go ("-f":file:args) cfg = go args cfg { input = File file }
        go ("-d":rootdir:args) cfg = go args cfg { dir = rootdir }
        go (arg@('-':_):_) _ = Left $ "Unknown Option: \"" ++ arg ++ "\""
        go args cfg = go args' =<< (addTest cfg <$> props)
            where
                (propNames, args') = break ((=='-') . head) args
                noPropErr name = "\"" ++ name ++ "\" is not a known Property"
                props = traverse (\name -> maybe (Left $ noPropErr name) Right
                                                 (lookup name properties)
                                 ) propNames
                addTest cfg p = cfg { tests = p ++ tests cfg }

fileFilter :: ([Rule String String] -> Bool) -> FilePath -> IO Bool
fileFilter p file = do
    problem <- fromFile file
    case problem of
      Left err -> hPrint stderr err >> return False
      Right prob -> return $ p (trs prob)
    where
        trs = allRules . rules

usage :: Handle -> IO ()
usage handle = do
    name <- getProgName
    hPutStrLn handle . unlines $
        [ "Usage: " ++ name ++ " OPTIONS [properties ..]"
        , ""
        , "OPTIONS:"
        , "\t-f inputfile\t\t file of newline separated .trs filenames (default: read from stdin)"
        , "\t-d dir\t\t\t directory where .trs files are located (default: .)"
        , ""
        , "Properties are:"
        ] ++ map (("\t"++) . fst) properties
