module Main where

import Control.Monad
import Data.Rewriting.Problem hiding (map)
import Data.Rewriting.Rule (Rule)
import Data.Foldable (traverse_)

import System.Directory (withCurrentDirectory)
import System.Environment
import System.Exit
import System.IO

import Properties hiding (map)

data Input = StdIn | File FilePath
    deriving Show

data Config = Config { input :: Input
                     , tests :: [[Rule String String] -> Bool]
                     , dir :: FilePath
                     }

defaultConfig = Config { input = StdIn
                       , tests = []
                       , dir = "./"
                       }

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ usage >> exitFailure
    hSetBuffering stdout LineBuffering
    case parseArgs args of
      Left err -> putStrLn err >> usage
      Right cfg -> run cfg

run :: Config -> IO ()
run cfg = do
    inp <- case input cfg of
             StdIn -> return stdin
             File file -> openFile file ReadMode
    trsFiles <- lines <$> hGetContents inp
    withCurrentDirectory (dir cfg) $
        forM_ trsFiles $ \file -> do
            res <- fileFilter (\x -> all ($ x) (tests cfg)) file
            when res $ putStrLn file
    hClose inp

parseArgs :: [String] -> Either String Config
parseArgs = flip go defaultConfig
    where
        go [] cfg = return cfg
        go ("-f":file:args) cfg = go args cfg { input = File file }
        go ("-d":rootdir:args) cfg = go args cfg { dir = rootdir }
        go args cfg =
            go args' =<< ((\p -> cfg { tests = p ++ tests cfg }) <$> props)
            where
                (propNames, args') = break ((=='-') . head) args
                noPropErr name = "\"" ++ name ++ "\" is not a known Property"
                props = traverse (\name -> maybe (Left $ noPropErr name) Right
                                                 (lookup name properties)
                                 ) propNames

fileFilter :: ([Rule String String] -> Bool) -> FilePath -> IO Bool
fileFilter p file = do
    problem <- fromFile file
    case problem of
      Left err -> hPrint stderr err >> return False
      Right prob -> return $ p (trs prob)
    where
        trs = allRules . rules

properties :: [(String, [Rule String String] -> Bool)]
properties =
    [ ("shallow", isShallow)
    , ("ground", isGround)
    , ("linear", isLinear)
    , ("right-reducible", isRightReducible)
    , ("flat", isFlat)
    ]

usage :: IO ()
usage = do
    name <- getProgName
    putStr . unlines $
        [ "Usage: " ++ name ++ " options [properties ..]"
        , ""
        , "Options:"
        , "\t-f inputfile\t\t file of newline separated .trs filenames (default: read from stdin)"
        , "\t-d dir\t\t\t directory where .trs files are located (default: .)"
        , ""
        , "Properties are:"
        ] ++ map (("\t"++) . fst) properties
