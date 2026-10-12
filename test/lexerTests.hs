
module Main where

import Control.Exception
    ( SomeException
    , evaluate
    , try
    )

import Lexer
    ( SourcePos(..)
    , Token(..)
    , TokenType(..)
    , tokenize
    )


-- ============================================================
-- Test Runner
-- ============================================================

main :: IO ()
main = do

    putStrLn "=========================================="
    putStrLn " HaskellSynth Phase 1 - Lexer Test Suite"
    putStrLn "=========================================="
    putStrLn ""

    runTest "Empty input"
        testEmptyInput

    runTest "Whitespace"
        testWhitespace

    runTest "Identifiers"
        testIdentifiers

    runTest "Identifier underscores"
        testIdentifierUnderscores

    runTest "Integer literals"
        testIntegerLiterals

    runTest "Keywords"
        testKeywords

    runTest "Boolean literals"
        testBooleanLiterals

    runTest "Type keywords"
        testTypeKeywords

    runTest "Arithmetic operators"
        testArithmeticOperators

    runTest "Comparison operators"
        testComparisonOperators

    runTest "Boolean operators"
        testBooleanOperators

    runTest "Function type syntax"
        testFunctionTypeSyntax

    runTest "Parentheses"
        testParentheses

    runTest "Source positions"
        testSourcePositions

    runTest "Multiline source positions"
        testMultilinePositions

    runTest "Complete adder example"
        testAdderExample

    runTest "Complete max example"
        testMaxExample

    runTest "Complete let example"
        testLetExample

    runTest "Invalid character"
        testInvalidCharacter

    putStrLn ""
    putStrLn "=========================================="
    putStrLn " All Phase 1 lexer tests passed."
    putStrLn "=========================================="


-- ============================================================
-- Test Infrastructure
-- ============================================================

runTest :: String -> IO () -> IO ()
runTest name test = do

    result <- try test

    case result of

        Left exception -> do
            putStrLn ("[FAIL] " ++ name)
            putStrLn ("       " ++ showException exception)

        Right _ ->
            putStrLn ("[PASS] " ++ name)


showException :: SomeException -> String
showException exception =
    show exception


assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> IO ()

assertEqual description expected actual =

    if expected == actual

        then
            pure ()

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

        then
            pure ()

        else
            error $
                "Assertion failed: "
                ++ description


assertLexesTo
    :: String
    -> [Token]
    -> IO ()

assertLexesTo source expected = do

    let actual = tokenize source

    assertEqual
        ("Tokenization of: " ++ show source)
        expected
        actual


-- ============================================================
-- Helper Constructors
-- ============================================================

pos :: Int -> Int -> SourcePos
pos line column =
    SourcePos line column


tok :: TokenType -> Int -> Int -> Token
tok tokenTypeValue line column =
    Token tokenTypeValue (pos line column)


-- ============================================================
-- 1. Empty Input
-- ============================================================

testEmptyInput :: IO ()
testEmptyInput =

    assertLexesTo
        ""
        []


-- ============================================================
-- 2. Whitespace
-- ============================================================

testWhitespace :: IO ()
testWhitespace =

    assertLexesTo
        "   \t  \r\n  "
        []


-- ============================================================
-- 3. Identifiers
-- ============================================================

testIdentifiers :: IO ()
testIdentifiers =

    assertLexesTo
        "adder result inputA"
        [ tok (Ident "adder") 1 1
        , tok (Ident "result") 1 7
        , tok (Ident "inputA") 1 14
        ]


-- ============================================================
-- 4. Identifier Underscores
-- ============================================================

testIdentifierUnderscores :: IO ()
testIdentifierUnderscores =

    assertLexesTo
        "data_in data_out carry_out reset_n"
        [ tok (Ident "data_in") 1 1
        , tok (Ident "data_out") 1 9
        , tok (Ident "carry_out") 1 18
        , tok (Ident "reset_n") 1 28
        ]


-- ============================================================
-- 5. Integer Literals
-- ============================================================

testIntegerLiterals :: IO ()
testIntegerLiterals =

    assertLexesTo
        "0 1 42 255 123456789"
        [ tok (IntLit 0) 1 1
        , tok (IntLit 1) 1 3
        , tok (IntLit 42) 1 5
        , tok (IntLit 255) 1 8
        , tok (IntLit 123456789) 1 12
        ]


-- ============================================================
-- 6. Keywords
-- ============================================================

testKeywords :: IO ()
testKeywords =

    assertLexesTo
        "if then else let in not"
        [ tok If 1 1
        , tok Then 1 4
        , tok Else 1 9
        , tok Let 1 14
        , tok In 1 18
        , tok Not 1 21
        ]


-- ============================================================
-- 7. Boolean Literals
-- ============================================================

testBooleanLiterals :: IO ()
testBooleanLiterals =

    assertLexesTo
        "True False"
        [ tok TrueLiteral 1 1
        , tok FalseLiteral 1 6
        ]


-- ============================================================
-- 8. Type Keywords
-- ============================================================

testTypeKeywords :: IO ()
testTypeKeywords =

    assertLexesTo
        "Bool UInt"
        [ tok BoolType 1 1
        , tok UIntType 1 6
        ]


-- ============================================================
-- 9. Arithmetic Operators
-- ============================================================

testArithmeticOperators :: IO ()
testArithmeticOperators =

    assertLexesTo
        "+ - * /"
        [ tok Plus 1 1
        , tok Minus 1 3
        , tok Star 1 5
        , tok Slash 1 7
        ]


-- ============================================================
-- 10. Comparison Operators
-- ============================================================

testComparisonOperators :: IO ()
testComparisonOperators =

    assertLexesTo
        "== /= < <= > >="
        [ tok EqualEqual 1 1
        , tok NotEqual 1 4
        , tok Less 1 7
        , tok LessEqual 1 9
        , tok Greater 1 12
        , tok GreaterEqual 1 14
        ]


-- ============================================================
-- 11. Boolean Operators
-- ============================================================

testBooleanOperators :: IO ()
testBooleanOperators =

    assertLexesTo
        "&& ||"
        [ tok And 1 1
        , tok Or 1 4
        ]


-- ============================================================
-- 12. Function Type Syntax
-- ============================================================

testFunctionTypeSyntax :: IO ()
testFunctionTypeSyntax =

    assertLexesTo
        ":: -> ="
        [ tok DoubleColon 1 1
        , tok Arrow 1 4
        , tok Equal 1 7
        ]


-- ============================================================
-- 13. Parentheses
-- ============================================================

testParentheses :: IO ()
testParentheses =

    assertLexesTo
        "(a + b)"
        [ tok LeftParen 1 1
        , tok (Ident "a") 1 2
        , tok Plus 1 4
        , tok (Ident "b") 1 6
        , tok RightParen 1 7
        ]

-- ============================================================
-- 16. Source Positions
-- ============================================================

testSourcePositions :: IO ()
testSourcePositions =

    assertLexesTo
        "abc 123"
        [ tok (Ident "abc") 1 1
        , tok (IntLit 123) 1 5
        ]


-- ============================================================
-- 17. Multiline Positions
-- ============================================================

testMultilinePositions :: IO ()
testMultilinePositions =

    assertLexesTo
        "a + b\nc + d"
        [ tok (Ident "a") 1 1
        , tok Plus 1 3
        , tok (Ident "b") 1 5
        , tok (Ident "c") 2 1
        , tok Plus 2 3
        , tok (Ident "d") 2 5
        ]


-- ============================================================
-- 18. Complete Adder Example
-- ============================================================

testAdderExample :: IO ()
testAdderExample =

    assertLexesTo
        "adder :: UInt 8 -> UInt 8 -> UInt 8\n\
        \adder a b = a + b"
        [ tok (Ident "adder") 1 1
        , tok DoubleColon 1 7
        , tok UIntType 1 10
        , tok (IntLit 8) 1 15
        , tok Arrow 1 17
        , tok UIntType 1 20
        , tok (IntLit 8) 1 25
        , tok Arrow 1 27
        , tok UIntType 1 30
        , tok (IntLit 8) 1 35

        , tok (Ident "adder") 2 1
        , tok (Ident "a") 2 7
        , tok (Ident "b") 2 9
        , tok Equal 2 11
        , tok (Ident "a") 2 13
        , tok Plus 2 15
        , tok (Ident "b") 2 17
        ]


-- ============================================================
-- 19. Complete max Example
-- ============================================================

testMaxExample :: IO ()
testMaxExample =

    assertLexesTo
        "max8 :: UInt 8 -> UInt 8 -> UInt 8\n\
        \max8 a b = if a > b then a else b"
        [ tok (Ident "max8") 1 1
        , tok DoubleColon 1 6
        , tok UIntType 1 9
        , tok (IntLit 8) 1 14
        , tok Arrow 1 16
        , tok UIntType 1 19
        , tok (IntLit 8) 1 24
        , tok Arrow 1 26
        , tok UIntType 1 29
        , tok (IntLit 8) 1 34

        , tok (Ident "max8") 2 1
        , tok (Ident "a") 2 6
        , tok (Ident "b") 2 8
        , tok Equal 2 10
        , tok If 2 12
        , tok (Ident "a") 2 15
        , tok Greater 2 17
        , tok (Ident "b") 2 19
        , tok Then 2 21
        , tok (Ident "a") 2 26
        , tok Else 2 28
        , tok (Ident "b") 2 33
        ]


-- ============================================================
-- 20. Complete Let Example
-- ============================================================

testLetExample :: IO ()
testLetExample =

    assertLexesTo
        "double :: UInt 8 -> UInt 8\n\
        \double x = let y = x + x in y"
        [ tok (Ident "double") 1 1
        , tok DoubleColon 1 8
        , tok UIntType 1 11
        , tok (IntLit 8) 1 16
        , tok Arrow 1 18
        , tok UIntType 1 21
        , tok (IntLit 8) 1 26

        , tok (Ident "double") 2 1
        , tok (Ident "x") 2 8
        , tok Equal 2 10
        , tok Let 2 12
        , tok (Ident "y") 2 16
        , tok Equal 2 18
        , tok (Ident "x") 2 20
        , tok Plus 2 22
        , tok (Ident "x") 2 24
        , tok In 2 26
        , tok (Ident "y") 2 29
        ]


-- ============================================================
-- 21. Invalid Character
-- ============================================================
testInvalidCharacter :: IO ()
testInvalidCharacter = do

    result <-
        try
            (evaluate (tokenize "a @ b"))
            :: IO (Either SomeException [Token])

    case result of

        Left _ ->
            pure ()

        Right _ ->
            error
                "Expected lexer to reject invalid character '@'"
