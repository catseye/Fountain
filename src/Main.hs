module Main (main) where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Fountain.Loader as Loader
import qualified Language.Fountain.Parser as Parser
import qualified Language.Fountain.Generator as Generator
import qualified Language.Fountain.Preprocessor as Preprocessor


data Flags = Flags {
    dumpState :: Bool,
    suppressNewline :: Bool
  } deriving (Show, Ord, Eq)

defaultFlags = Flags{ dumpState = False, suppressNewline = False }

parseFlags flags ("--dump-state":rest) =
    parseFlags flags{ dumpState = True } rest
parseFlags flags ("--suppress-newline":rest) =
    parseFlags flags{ suppressNewline = True } rest
parseFlags flags other = (flags, other)


main = do
    args <- getArgs
    let (flags, args') = parseFlags defaultFlags args
    let output = if (suppressNewline flags) then putStr else putStrLn
    case args' of
        ["load", grammarFileName] -> do
            grammar <- loadSource grammarFileName
            output $ show grammar
        ["preprocess", grammarFileName] -> do
            grammar <- loadSource grammarFileName
            let grammar' = Preprocessor.preprocessGrammar grammar
            output $ show grammar'
        ("parse":grammarFileName:textFileName:initialParams) -> do
            grammar <- loadSource grammarFileName
            text <- loadText textFileName
            let initialState = Parser.constructState text initialParams
            let finalState = Parser.parseFrom grammar initialState
            output $ if (dumpState flags) then show finalState else formatParseResult $ Parser.obtainResult finalState
            exitWith $ either (\_msg -> ExitFailure 1) (\_remaining -> ExitSuccess) $ Parser.obtainResult finalState
        ("generate":grammarFileName:initialParams) -> do
            grammar <- loadSource grammarFileName
            let grammar' = Preprocessor.preprocessGrammar grammar
            let initialState = Generator.constructState initialParams
            let finalState = Generator.generateFrom grammar' initialState
            output $ if (dumpState flags) then show finalState else formatGenerateResult $ Generator.obtainResult finalState
            exitWith $ either (\_msg -> ExitFailure 1) (\_remaining -> ExitSuccess) $ Generator.obtainResult finalState
        _ -> usage

usage = abortWith
    (
        "Usage:\n" ++
        "    fountain {flags} load <fountain-filename>\n" ++
        "    fountain {flags} preprocess <fountain-filename>\n" ++
        "    fountain {flags} parse <fountain-filename> <text-filename> {params}\n" ++
        "    fountain {flags} generate <fountain-filename> {params}\n" ++
        "  where {flags} is any of:\n" ++
        "    --dump-state: dump the internal parse/generate state as part of output\n" ++
        "    --suppress-newline: don't output a final newline after output\n" ++
        "  and {params} is a list of arguments of the form `var=value` with which\n" ++
        "    variables will be initialized in the initial parse/generate state."
    )

loadSource fileName = do
    handle <- openFile fileName ReadMode
    -- hSetEncoding handle utf8
    text <- hGetContents handle
    case Loader.parseFountain text of
        Right g -> do
            return g
        Left err ->
            abortWith $ show err

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
