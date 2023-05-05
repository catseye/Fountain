module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Fountain.Loader as Loader
import qualified Language.Fountain.Parser as Parser
import qualified Language.Fountain.Generator as Generator
import qualified Language.Fountain.Preprocessor as Preprocessor


data Flags = Flags {
    dumpState :: Bool
  } deriving (Show, Ord, Eq)

defaultFlags = Flags{ dumpState = False }

parseFlags flags ("--dump-state":rest) =
    parseFlags flags{ dumpState = True } rest
parseFlags flags other = (flags, other)


main = do
    args <- getArgs
    let (flags, args') = parseFlags defaultFlags args
    case args' of
        ["load", grammarFileName] -> do
            grammar <- loadSource grammarFileName
            putStrLn $ show grammar
        ["preprocess", grammarFileName] -> do
            grammar <- loadSource grammarFileName
            let grammar' = Preprocessor.preprocessGrammar grammar
            putStrLn $ show grammar'
        ["parse", grammarFileName, textFileName] -> do
            grammar <- loadSource grammarFileName
            text <- loadText textFileName
            let finalState = Parser.parseFrom grammar text
            putStrLn $ if (dumpState flags) then show finalState else formatParseResult $ Parser.obtainResult finalState
            exitWith $ either (\msg -> ExitFailure 1) (\remaining -> ExitSuccess) $ Parser.obtainResult finalState
        ["generate", grammarFileName] -> do
            grammar <- loadSource grammarFileName
            let grammar' = Preprocessor.preprocessGrammar grammar
            let finalState = Generator.generateFrom grammar'
            putStrLn $ if (dumpState flags) then show finalState else formatGenerateResult $ Generator.obtainResult finalState
            exitWith $ either (\msg -> ExitFailure 1) (\remaining -> ExitSuccess) $ Generator.obtainResult finalState
        _ -> do
            abortWith "Usage: fountain {flags} (load|preprocess|parse|generate) <input-filename> [<input-text>]"


loadSource fileName = do
    handle <- openFile fileName ReadMode
    -- hSetEncoding handle utf8
    text <- hGetContents handle
    case Loader.parseFountain text of
        Right g -> do
            return g
        Left error ->
            abortWith $ show error

loadText fileName = do
    handle <- if fileName == "--" then return stdin else openFile fileName ReadMode
    -- hSetEncoding handle utf8
    text <- hGetContents handle
    return text

abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

formatParseResult (Right "") = "Success"
formatParseResult (Right s) = "Remaining: " ++ (show s)
formatParseResult (Left _) = "Failure"

formatGenerateResult (Right s) = s
formatGenerateResult (Left _) = "Failure"
