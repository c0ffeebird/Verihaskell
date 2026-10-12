module Parser
    ( parseProgram
    ) where

import AST
import Lexer
import ParserCore


parseProgram
    :: String
    -> Either ParseError Program

parseProgram source =
    parseAll
        programParser
        (tokenize source)


programParser :: Parser Program
programParser =
    mapParser
        Program
        (manyP functionParser)


functionParser :: Parser Function
functionParser =
    mapParser
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


typeSignatureParser
    :: Parser (String, Type)

typeSignatureParser =
    mapParser
        extract
        (
            identifier
                `andThen`
            (
                token DoubleColon
                    `andThen`
                typeParser
            )
        )
  where
    extract
        (name, (_, typeValue)) =
        (name, typeValue)


typeParser :: Parser Type
typeParser =
    mapParser
        buildType
        (
            simpleTypeParser
                `andThen`
            optionalP
                (
                    token Arrow
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
simpleTypeParser =
    boolTypeParser
        `orElse`
    uintTypeParser


boolTypeParser :: Parser Type
boolTypeParser =
    mapParser
        (\_ -> BoolType)
        (token BoolType)


uintTypeParser :: Parser Type
uintTypeParser =
    mapParser
        build
        (
            token UIntType
                `andThen`
            integer
        )
  where
    build (_, width) =
        UIntType (fromInteger width)


functionEquationParser
    :: Parser (String, [Parameter], Expr)

functionEquationParser =
    mapParser
        build
        (
            identifier
                `andThen`
            (
                manyP parameterParser
                    `andThen`
                (
                    token Equal
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
parameterParser =
    mapParser
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
ifParser =
    mapParser
        build
        (
            token If
                `andThen`
            (
                expressionParser
                    `andThen`
                (
                    token Then
                        `andThen`
                    (
                        expressionParser
                            `andThen`
                        (
                            token Else
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
letParser =
    mapParser
        build
        (
            token Let
                `andThen`
            (
                identifier
                    `andThen`
                (
                    token Equal
                        `andThen`
                    (
                        expressionParser
                            `andThen`
                        (
                            token In
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
notParser =
    mapParser
        (\(_, expression) -> Not expression)
        (
            token Not
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
        (token LeftParen)
        (token RightParen)
        expressionParser


addOperator
    :: Parser (Expr -> Expr -> Expr)

addOperator =
    mapParser
        (\_ -> Add)
        (token Plus)
        `orElse`
    mapParser
        (\_ -> Sub)
        (token Minus)


mulOperator
    :: Parser (Expr -> Expr -> Expr)

mulOperator =
    mapParser
        (\_ -> Mul)
        (token Star)
        `orElse`
    mapParser
        (\_ -> Div)
        (token Slash)


relationalOperator
    :: Parser (Expr -> Expr -> Expr)

relationalOperator =
    mapParser
        (\_ -> LessEqual)
        (token LessEqual)
        `orElse`
    mapParser
        (\_ -> GreaterEqual)
        (token GreaterEqual)
        `orElse`
    mapParser
        (\_ -> Less)
        (token Less)
        `orElse`
    mapParser
        (\_ -> Greater)
        (token Greater)


equalityOperator
    :: Parser (Expr -> Expr -> Expr)

equalityOperator =
    mapParser
        (\_ -> Equal)
        (token EqualEqual)
        `orElse`
    mapParser
        (\_ -> NotEqual)
        (token NotEqual)


andOperator
    :: Parser (Expr -> Expr -> Expr)

andOperator =
    mapParser
        (\_ -> And)
        (token And)


orOperator
    :: Parser (Expr -> Expr -> Expr)

orOperator =
    mapParser
        (\_ -> Or)
        (token Or)


leftAssociative
    :: Parser a
    -> Parser (a -> a -> a)
    -> Parser a

leftAssociative operand operator =
    mapParser
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
    build (firstValue, rest) =
        foldLeft firstValue rest

    foldLeft value [] =
        value

    foldLeft value ((operation, nextValue) : rest) =
        foldLeft
            (operation value nextValue)
            rest
