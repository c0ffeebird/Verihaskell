
module Main where

import AST
import Parser


main :: IO ()
main = do
    testSimpleAdder
    testTypeSignature
    testArithmeticPrecedence
    testBooleanExpression
    testIfExpression
    testLetExpression
    testFunctionApplication
    testParentheses
    testNotExpression
    testMultipleFunctions

    putStrLn ""
    putStrLn "All Parser tests passed."


assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> IO ()

assertEqual testName expected actual =
    if expected == actual
        then
            putStrLn
                ("[PASS] " ++ testName)
        else
            error $
                "[FAIL] " ++ testName
                ++ "\nExpected: "
                ++ show expected
                ++ "\nActual:   "
                ++ show actual


assertParse
    :: String
    -> Program
    -> IO ()

assertParse testName expected source =
    case parseProgram source of
        Left parseError ->
            error $
                "[FAIL] " ++ testName
                ++ "\nParser returned an error: "
                ++ show parseError

        Right actual ->
            assertEqual
                testName
                expected
                actual


testSimpleAdder :: IO ()
testSimpleAdder = do
    let source =
            "adder :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "adder a b = a + b"

    let expected =
            Program
                [ Function
                    "adder"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Add
                        (Var "a")
                        (Var "b")
                    )
                ]

    assertParse
        "Simple adder"
        expected
        source


testTypeSignature :: IO ()
testTypeSignature = do
    let source =
            "isPositive :: UInt 8 -> Bool\n"
            ++ "isPositive x = x > 0"

    let expected =
            Program
                [ Function
                    "isPositive"
                    [ Parameter "x"
                    ]
                    (FunctionType
                        (UIntType 8)
                        BoolType
                    )
                    (Greater
                        (Var "x")
                        (UIntLiteral 0)
                    )
                ]

    assertParse
        "Type signature"
        expected
        source


testArithmeticPrecedence :: IO ()
testArithmeticPrecedence = do
    let source =
            "calculate :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "calculate a b = a + b * 2"

    let expected =
            Program
                [ Function
                    "calculate"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Add
                        (Var "a")
                        (Mul
                            (Var "b")
                            (UIntLiteral 2)
                        )
                    )
                ]

    assertParse
        "Arithmetic precedence"
        expected
        source


testBooleanExpression :: IO ()
testBooleanExpression = do
    let source =
            "between :: UInt 8 -> UInt 8 -> UInt 8 -> Bool\n"
            ++ "between x low high = x >= low && x <= high"

    let expected =
            Program
                [ Function
                    "between"
                    [ Parameter "x"
                    , Parameter "low"
                    , Parameter "high"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (FunctionType
                                (UIntType 8)
                                BoolType
                            )
                        )
                    )
                    (And
                        (GreaterEqual
                            (Var "x")
                            (Var "low")
                        )
                        (LessEqual
                            (Var "x")
                            (Var "high")
                        )
                    )
                ]

    assertParse
        "Boolean expression"
        expected
        source


testIfExpression :: IO ()
testIfExpression = do
    let source =
            "max8 :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "max8 a b = if a > b then a else b"

    let expected =
            Program
                [ Function
                    "max8"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (If
                        (Greater
                            (Var "a")
                            (Var "b")
                        )
                        (Var "a")
                        (Var "b")
                    )
                ]

    assertParse
        "If expression"
        expected
        source


testLetExpression :: IO ()
testLetExpression = do
    let source =
            "doubleAdd :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "doubleAdd a b = let x = a + b in x * 2"

    let expected =
            Program
                [ Function
                    "doubleAdd"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Let
                        "x"
                        (Add
                            (Var "a")
                            (Var "b")
                        )
                        (Mul
                            (Var "x")
                            (UIntLiteral 2)
                        )
                    )
                ]

    assertParse
        "Let expression"
        expected
        source


testFunctionApplication :: IO ()
testFunctionApplication = do
    let source =
            "compute :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "compute a b = max8 a b"

    let expected =
            Program
                [ Function
                    "compute"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Call
                        (Var "max8")
                        [ Var "a"
                        , Var "b"
                        ]
                    )
                ]

    assertParse
        "Function application"
        expected
        source


testParentheses :: IO ()
testParentheses = do
    let source =
            "calculate :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "calculate a b = (a + b) * 2"

    let expected =
            Program
                [ Function
                    "calculate"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Mul
                        (Add
                            (Var "a")
                            (Var "b")
                        )
                        (UIntLiteral 2)
                    )
                ]

    assertParse
        "Parentheses"
        expected
        source


testNotExpression :: IO ()
testNotExpression = do
    let source =
            "notEqual :: UInt 8 -> UInt 8 -> Bool\n"
            ++ "notEqual a b = not (a == b)"

    let expected =
            Program
                [ Function
                    "notEqual"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            BoolType
                        )
                    )
                    (Not
                        (Equal
                            (Var "a")
                            (Var "b")
                        )
                    )
                ]

    assertParse
        "Not expression"
        expected
        source


testMultipleFunctions :: IO ()
testMultipleFunctions = do
    let source =
            "add :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "add a b = a + b\n"
            ++ "\n"
            ++ "max8 :: UInt 8 -> UInt 8 -> UInt 8\n"
            ++ "max8 a b = if a > b then a else b"

    let expected =
            Program
                [ Function
                    "add"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (Add
                        (Var "a")
                        (Var "b")
                    )

                , Function
                    "max8"
                    [ Parameter "a"
                    , Parameter "b"
                    ]
                    (FunctionType
                        (UIntType 8)
                        (FunctionType
                            (UIntType 8)
                            (UIntType 8)
                        )
                    )
                    (If
                        (Greater
                            (Var "a")
                            (Var "b")
                        )
                        (Var "a")
                        (Var "b")
                    )
                ]

    assertParse
        "Multiple functions"
        expected
        source

