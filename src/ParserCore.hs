module ParserCore
    ( Parser
    , ParseError(..)
    , runParser
    , parseAll

    , mapParser
    , andThen
    , orElse

    , manyP
    , someP
    , optionalP

    , satisfyToken
    , token

    , identifier
    , integer
    , boolean

    , between
    ) where

import Lexer
    ( SourcePos(..)
    , Token(..)
    , TokenType(..)
    )


data ParseError
    = ParseError
        SourcePos
        String
    deriving (Show, Eq)


newtype Parser a =
    Parser
        { runParser
            :: [Token]
            -> Either ParseError (a, [Token])
        }


parseAll :: Parser a -> [Token] -> Either ParseError a
parseAll parser tokens =
    case runParser parser tokens of
        Left err ->
            Left err

        Right (value, []) ->
            Right value

        Right (_, remaining@(Token _ position : _)) ->
            Left
                (ParseError
                    position
                    "unexpected token after complete parse"
                )


mapParser
    :: (a -> b)
    -> Parser a
    -> Parser b

mapParser transform parser =
    Parser $ \tokens ->
        case runParser parser tokens of
            Left err ->
                Left err

            Right (value, remaining) ->
                Right
                    (transform value, remaining)


andThen
    :: Parser a
    -> Parser b
    -> Parser (a, b)

andThen parserA parserB =
    Parser $ \tokens ->
        case runParser parserA tokens of
            Left err ->
                Left err

            Right (valueA, remainingA) ->
                case runParser parserB remainingA of
                    Left err ->
                        Left err

                    Right (valueB, remainingB) ->
                        Right
                            ( (valueA, valueB)
                            , remainingB
                            )


orElse :: Parser a -> Parser a -> Parser a
orElse parserA parserB =
    Parser $ \tokens ->
        case runParser parserA tokens of
            Right result ->
                Right result

            Left _ ->
                runParser parserB tokens


manyP :: Parser a -> Parser [a]
manyP parser =
    Parser $ \tokens ->
        collect tokens []
  where
    collect remaining accumulated =
        case runParser parser remaining of

            Left _ ->
                Right
                    ( reverse accumulated
                    , remaining
                    )

            Right (value, newRemaining) ->

                if newRemaining == remaining
                    then
                        error
                            "manyP: parser accepted input without consuming a token"

                    else
                        collect
                            newRemaining
                            (value : accumulated)


someP :: Parser a -> Parser [a]
someP parser =
    mapParser
        (\(firstValue, restValues) ->
            firstValue : restValues)
        (parser `andThen` manyP parser)


optionalP :: Parser a -> Parser (Maybe a)
optionalP parser =
    Parser $ \tokens ->
        case runParser parser tokens of
            Right (value, remaining) ->
                Right
                    (Just value, remaining)

            Left _ ->
                Right
                    (Nothing, tokens)


satisfyToken :: (TokenType -> Bool) -> String -> Parser Token
satisfyToken predicate expected =
    Parser $ \tokens ->
        case tokens of

            [] ->
                Left
                    (ParseError
                        (SourcePos 0 0)
                        ("expected " ++ expected
                            ++ ", but reached end of input")
                    )

            tokenValue@(Token tokenTypeValue position) : rest ->

                if predicate tokenTypeValue
                    then
                        Right
                            (tokenValue, rest)

                    else
                        Left
                            (ParseError
                                position
                                ("expected " ++ expected
                                    ++ ", found "
                                    ++ show tokenTypeValue)
                            )


token :: TokenType -> Parser Token
token expectedToken =
    satisfyToken
        (== expectedToken)
        (show expectedToken)


identifier :: Parser String
identifier = mapParser
        extractIdentifier
        (satisfyToken isIdentifier "identifier")
  where
    isIdentifier tokenTypeValue =
        case tokenTypeValue of
            Ident _ ->
                True

            _ ->
                False

    extractIdentifier tokenValue =
        case tokenType tokenValue of
            Ident name ->
                name

            _ ->
                error
                    "identifier: impossible token"


integer :: Parser Integer
integer =
    mapParser
        extractInteger
        (satisfyToken isInteger "integer literal")
  where
    isInteger tokenTypeValue =
        case tokenTypeValue of
            IntLit _ ->
                True

            _ ->
                False

    extractInteger tokenValue =
        case tokenType tokenValue of
            IntLit value ->
                value

            _ ->
                error
                    "integer: impossible token"


boolean :: Parser Bool
boolean = mapParser
        extractBoolean
        (satisfyToken isBoolean "boolean literal")
  where
    isBoolean tokenTypeValue =
        case tokenTypeValue of
            TrueLiteral ->
                True

            FalseLiteral ->
                True

            _ ->
                False

    extractBoolean tokenValue =
        case tokenType tokenValue of
            TrueLiteral ->
                True

            FalseLiteral ->
                False

            _ ->
                error
                    "boolean: impossible token"


between :: Parser open -> Parser close -> Parser a -> Parser a
between open close parser =
    mapParser
        extractMiddle
        (
            open
                `andThen`
            (
                parser
                    `andThen`
                close
            )
        )
  where
    extractMiddle
        (_, (value, _)) =
        value
