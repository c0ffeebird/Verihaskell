
module Parser
    ( parseProgram
    ) where

import AST
import qualified Lexer as L
import ParserCore


parseProgram :: String -> Either ParseError Program
parseProgram source = parseAll programParser (L.tokenize source)


programParser :: Parser Program
programParser = mapParser Program (manyP functionParser)


functionParser :: Parser Function
functionParser = mapParser
        build
        (
            typeSignatureParser
                `andThen`
            functionEquationParser
        )
  where
    build
        ((signatureName, functionType),
         (equationName, parameters, body)) =

        if signatureName == equationName
            then
                Function
                    equationName
                    parameters
                    functionType
                    body
            else
                error
                    "function name mismatch"


typeSignatureParser :: Parser (String, Type)
typeSignatureParser = mapParser
        extract
        (
            identifier
                `andThen`
            (
                token L.DoubleColon
                    `andThen`
                typeParser
            )
        )
  where
    extract
        (name, (_, typeValue)) =
        (name, typeValue)


typeParser :: Parser Type
typeParser = mapParser
        buildType
        (
            simpleTypeParser
                `andThen`
            optionalP
                (
                    token L.Arrow
                        `andThen`
                    typeParser
                )
        )
  where
    buildType
        (baseType, Nothing) =
        baseType

    buildType
        (baseType, Just (_, returnType)) =
        FunctionType baseType returnType


simpleTypeParser :: Parser Type
simpleTypeParser = boolTypeParser `orElse` uintTypeParser


boolTypeParser :: Parser Type
boolTypeParser = mapParser (\_ -> BoolType) (token L.BoolType)


uintTypeParser :: Parser Type
uintTypeParser = mapParser
        build
        (
            token L.UIntType
                `andThen`
            integer
        )
  where
    build (_, width) =
        UIntType (fromInteger width)


functionEquationParser :: Parser (String, [Parameter], Expr)
functionEquationParser = mapParser
        build
        (
            identifier
                `andThen`
            (
                manyP parameterParser
                    `andThen`
                (
                    token L.Equal
                        `andThen`
                    expressionParser
                )
            )
        )
  where
    build
        (name, (parameters, (_, body))) =
        (name, parameters, body)


parameterParser :: Parser Parameter
parameterParser = mapParser
        Parameter
        identifier


expressionParser :: Parser Expr
expressionParser =
    ifParser
        `orElse`
    letParser
        `orElse`
    orParser


ifParser :: Parser Expr
ifParser = mapParser
        build
        (
            token L.If
                `andThen`
            (
                expressionParser
                    `andThen`
                (
                    token L.Then
                        `andThen`
                    (
                        expressionParser
                            `andThen`
                        (
                            token L.Else
                                `andThen`
                            expressionParser
                        )
                    )
                )
            )
        )
  where
    build
        (_, (condition,
            (_, (thenBranch,
                (_, elseBranch))))) =

        If
            condition
            thenBranch
            elseBranch


letParser :: Parser Expr
letParser = mapParser
        build
        (
            token L.Let
                `andThen`
            (
                identifier
                    `andThen`
                (
                    token L.Equal
                        `andThen`
                    (
                        expressionParser
                            `andThen`
                        (
                            token L.In
                                `andThen`
                            expressionParser
                        )
                    )
                )
            )
        )
  where
    build
        (_, (name,
            (_, (boundValue,
                (_, body))))) =

        Let
            name
            boundValue
            body


orParser :: Parser Expr
orParser =
    leftAssociative
        andParser
        orOperator


andParser :: Parser Expr
andParser =
    leftAssociative
        equalityParser
        andOperator


equalityParser :: Parser Expr
equalityParser =
    leftAssociative
        relationalParser
        equalityOperator


relationalParser :: Parser Expr
relationalParser =
    leftAssociative
        additiveParser
        relationalOperator


additiveParser :: Parser Expr
additiveParser =
    leftAssociative
        multiplicativeParser
        addOperator


multiplicativeParser :: Parser Expr
multiplicativeParser =
    leftAssociative
        applicationParser
        mulOperator


applicationParser :: Parser Expr
applicationParser =
    mapParser
        build
        (
            unaryParser
                `andThen`
            manyP unaryParser
        )
  where
    build
        (functionExpr, arguments) =

        case arguments of
            [] ->
                functionExpr

            _ ->
                Call
                    functionExpr
                    arguments


unaryParser :: Parser Expr
unaryParser =
    notParser
        `orElse`
    primaryParser


notParser :: Parser Expr
notParser = mapParser
        (\(_, expression) -> Not expression)
        (
            token L.Not
                `andThen`
            unaryParser
        )


primaryParser :: Parser Expr
primaryParser =
    integerParser
        `orElse`
    booleanParser
        `orElse`
    variableParser
        `orElse`
    parenthesizedParser


integerParser :: Parser Expr
integerParser =
    mapParser
        UIntLiteral
        integer


booleanParser :: Parser Expr
booleanParser =
    mapParser
        BoolLiteral
        boolean


variableParser :: Parser Expr
variableParser =
    mapParser
        Var
        identifier


parenthesizedParser :: Parser Expr
parenthesizedParser =
    between
        (token L.LeftParen)
        (token L.RightParen)
        expressionParser


addOperator :: Parser (Expr -> Expr -> Expr)
addOperator = mapParser
        (\_ -> Add)
        (token L.Plus)
        `orElse`
    mapParser
        (\_ -> Sub)
        (token L.Minus)


mulOperator :: Parser (Expr -> Expr -> Expr)
mulOperator = mapParser
        (\_ -> Mul)
        (token L.Star)
        `orElse`
    mapParser
        (\_ -> Div)
        (token L.Slash)


relationalOperator :: Parser (Expr -> Expr -> Expr)
relationalOperator =
    mapParser
        (\_ -> LessEqual)
        (token L.LessEqual)
        `orElse`
    mapParser
        (\_ -> GreaterEqual)
        (token L.GreaterEqual)
        `orElse`
    mapParser
        (\_ -> Less)
        (token L.Less)
        `orElse`
    mapParser
        (\_ -> Greater)
        (token L.Greater)


equalityOperator :: Parser (Expr -> Expr -> Expr)
equalityOperator =
    mapParser
        (\_ -> Equal)
        (token L.EqualEqual)
        `orElse`
    mapParser
        (\_ -> NotEqual)
        (token L.NotEqual)


andOperator :: Parser (Expr -> Expr -> Expr)
andOperator =
    mapParser
        (\_ -> And)
        (token L.And)


orOperator :: Parser (Expr -> Expr -> Expr)
orOperator =
    mapParser
        (\_ -> Or)
        (token L.Or)


leftAssociative :: Parser a -> Parser (a -> a -> a) -> Parser a
leftAssociative operand operator = mapParser
        build
        (
            operand
                `andThen`
            manyP
                (
                    operator
                        `andThen`
                    operand
                )
        )
  where
    build
        (firstValue, rest) =
        foldLeft firstValue rest

    foldLeft
        value
        [] =
        value

    foldLeft
        value
        ((operation, nextValue) : rest) =
        foldLeft
            (operation value nextValue)
            rest

