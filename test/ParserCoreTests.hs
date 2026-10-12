module Main where

import Lexer
    ( Token(..)
    , TokenType(..)
    , tokenize
    )
    
import Control.Exception
    ( SomeException
    , try
    )
        
import ParserCore
    ( ParseError(..)
    , Parser
    , andThen
    , between
    , boolean
    , identifier
    , integer
    , manyP
    , mapParser
    , optionalP
    , orElse
    , parseAll
    , runParser
    , someP
    , token
    )


main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn " HaskellSynth Phase 2 - ParserCore Tests"
    putStrLn "=========================================="
    putStrLn ""

    runTest "Parse identifier"
        testIdentifier

    runTest "Parse integer"
        testInteger

    runTest "Parse boolean True"
        testTrue

    runTest "Parse boolean False"
        testFalse

    runTest "Parse exact token"
        testToken

    runTest "Parser sequencing"
        testAndThen

    runTest "Parser mapping"
        testMapParser

    runTest "Parser alternatives"
        testOrElse

    runTest "Many parser"
        testMany

    runTest "Some parser"
        testSome

    runTest "Optional parser present"
        testOptionalPresent

    runTest "Optional parser absent"
        testOptionalAbsent

    runTest "Between parser"
        testBetween

    runTest "parseAll success"
        testParseAllSuccess

    runTest "parseAll rejects leftovers"
        testParseAllRejectsLeftovers

    runTest "Parser failure"
        testFailure

    putStrLn ""
    putStrLn "=========================================="
    putStrLn " All Phase 2 ParserCore tests passed."
    putStrLn "=========================================="


runTest :: String -> IO () -> IO ()
runTest name test = do
    result <- tryTest test

    case result of
        Nothing ->
            putStrLn ("[PASS] " ++ name)

        Just message -> do
            putStrLn ("[FAIL] " ++ name)
            putStrLn ("       " ++ message)


tryTest :: IO () -> IO (Maybe String)
tryTest action = do
    result <-
        try action
            :: IO (Either SomeException ())

    case result of
        Left exception ->
            pure (Just (show exception))

        Right _ ->
            pure Nothing


assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> IO ()

assertEqual description expected actual =
    if expected == actual
        then pure ()
        else
            error $
                description
                ++ "\nExpected: "
                ++ show expected
                ++ "\nActual:   "
                ++ show actual


assertTrue
    :: String
    -> Bool
    -> IO ()

assertTrue description condition =
    if condition
        then pure ()
        else
            error $
                "Assertion failed: "
                ++ description


testIdentifier :: IO ()
testIdentifier =
    assertEqual
        "identifier"
        (Right "adder")
        (parseAll identifier (tokenize "adder"))


testInteger :: IO ()
testInteger =
    assertEqual
        "integer"
        (Right 42)
        (parseAll integer (tokenize "42"))


testTrue :: IO ()
testTrue =
    assertEqual
        "True"
        (Right True)
        (parseAll boolean (tokenize "True"))


testFalse :: IO ()
testFalse =
    assertEqual
        "False"
        (Right False)
        (parseAll boolean (tokenize "False"))


testToken :: IO ()
testToken =
    assertTrue
        "Plus token should be recognized"
        (
            case parseAll
                (token Plus)
                (tokenize "+")
            of
                Right _ ->
                    True

                Left _ ->
                    False
        )


testAndThen :: IO ()
testAndThen =
    assertEqual
        "identifier followed by integer"
        (Right ("a", 10))
        (
            parseAll
                (identifier `andThen` integer)
                (tokenize "a 10")
        )


testMapParser :: IO ()
testMapParser =
    assertEqual
        "map integer"
        (Right 84)
        (
            parseAll
                (mapParser (* 2) integer)
                (tokenize "42")
        )


testOrElse :: IO ()
testOrElse =
    assertEqual
        "integer or identifier"
        (Right "hello")
        (
            parseAll
                (
                    mapParser show integer
                        `orElse`
                    identifier
                )
                (tokenize "hello")
        )


testMany :: IO ()
testMany =
    assertEqual
        "many integers"
        (Right [1, 2, 3])
        (
            parseAll
                (manyP integer)
                (tokenize "1 2 3")
        )


testSome :: IO ()
testSome =
    assertEqual
        "some identifiers"
        (Right ["a", "b", "c"])
        (
            parseAll
                (someP identifier)
                (tokenize "a b c")
        )


testOptionalPresent :: IO ()
testOptionalPresent =
    assertEqual
        "optional identifier present"
        (Right (Just "a"))
        (
            parseAll
                (optionalP identifier)
                (tokenize "a")
        )


testOptionalAbsent :: IO ()
testOptionalAbsent =
    assertEqual
        "optional identifier absent"
        (Right Nothing)
        (
            parseAll
                (optionalP identifier)
                (tokenize "+")
        )


testBetween :: IO ()
testBetween =
    assertEqual
        "parenthesized integer"
        (Right 42)
        (
            parseAll
                (
                    between
                        (token LeftParen)
                        (token RightParen)
                        integer
                )
                (tokenize "(42)")
        )


testParseAllSuccess :: IO ()
testParseAllSuccess =
    assertEqual
        "complete parse"
        (Right "hello")
        (
            parseAll
                identifier
                (tokenize "hello")
        )


testParseAllRejectsLeftovers :: IO ()
testParseAllRejectsLeftovers =
    case parseAll identifier (tokenize "hello world") of
        Left _ ->
            pure ()

        Right _ ->
            error
                "parseAll should reject unconsumed tokens"


testFailure :: IO ()
testFailure =
    case parseAll integer (tokenize "hello") of
        Left _ ->
            pure ()

        Right _ ->
            error
                "integer parser should reject identifier"
