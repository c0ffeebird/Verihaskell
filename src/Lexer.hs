
module Lexer
    ( TokenType(..)
    , SourcePos(..)
    , Token(..)
    , tokenize
    ) where

import Data.Char
    ( isAlpha
    , isAlphaNum
    , isDigit
    )


data TokenType
    = If
    | Then
    | Else
    | Let
    | In

    | TrueLiteral
    | FalseLiteral
    | Not

    | BoolType
    | UIntType

    | Ident String
    | IntLit Integer

    | Plus
    | Minus
    | Star
    | Slash

    | EqualEqual
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual

    | And
    | Or

    | DoubleColon
    | Arrow
    | Equal

    | LeftParen
    | RightParen

    deriving (Show, Eq)



data SourcePos = SourcePos
    { sourceLine   :: Int
    , sourceColumn :: Int
    }
    deriving (Show, Eq)


data Token = Token
    { tokenType :: TokenType
    , tokenPos  :: SourcePos
    }
    deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize source =lexTokens source (SourcePos 1 1)


lexTokens :: String -> SourcePos -> [Token]
lexTokens [] _ = []
lexTokens input@(c : cs) pos

    | c == ' ' || c == '\t' || c == '\r' =
        lexTokens cs (advanceColumn pos)

    | c == '\n' =
        lexTokens cs (newLine pos)

    | c == ':' && startsWith ':' cs =
        emit DoubleColon pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '-' && startsWith '>' cs =
        emit Arrow pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '=' && startsWith '=' cs =
        emit EqualEqual pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '/' && startsWith '=' cs =
        emit NotEqual pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '<' && startsWith '=' cs =
        emit LessEqual pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '>' && startsWith '=' cs =
        emit GreaterEqual pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '&' && startsWith '&' cs =
        emit And pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '|' && startsWith '|' cs =
        emit Or pos
            (drop 1 cs)
            (advanceColumnN pos 2)

    | c == '+' =
        emit Plus pos
            cs
            (advanceColumn pos)

    | c == '-' =
        emit Minus pos
            cs
            (advanceColumn pos)

    | c == '*' =
        emit Star pos
            cs
            (advanceColumn pos)

    | c == '/' =
        emit Slash pos
            cs
            (advanceColumn pos)

    | c == '=' =
        emit Equal pos
            cs
            (advanceColumn pos)

    | c == '<' =
        emit Less pos
            cs
            (advanceColumn pos)

    | c == '>' =
        emit Greater pos
            cs
            (advanceColumn pos)

    | c == '(' =
        emit LeftParen pos
            cs
            (advanceColumn pos)

    | c == ')' =
        emit RightParen pos
            cs
            (advanceColumn pos)

    | isDigit c =
        lexInteger input pos

    | isIdentifierStart c =
        lexIdentifier input pos

    | otherwise =
        lexicalError
            ("invalid character '" ++ [c] ++ "'")
            pos


emit :: TokenType-> SourcePos-> String-> SourcePos-> [Token]
emit tokenTypeValue tokenStart rest newPos = Token tokenTypeValue tokenStart : lexTokens rest newPos


lexInteger :: String-> SourcePos-> [Token]
lexInteger input pos =

    let
        (digits, rest) =
            span isDigit input

        value =
            read digits :: Integer

        newPos =
            advanceColumnN pos (length digits)

    in
        Token (IntLit value) pos
            : lexTokens rest newPos

lexIdentifier :: String-> SourcePos-> [Token]
lexIdentifier input pos =

    let
        (word, rest) =
            span isIdentifierChar input

        tokenTypeValue =
            keywordOrIdentifier word

        newPos =
            advanceColumnN pos (length word)

    in
        Token tokenTypeValue pos
            : lexTokens rest newPos

isIdentifierStart :: Char -> Bool
isIdentifierStart c =
    isAlpha c


isIdentifierChar :: Char -> Bool
isIdentifierChar c =
    isAlphaNum c || c == '_'

keywordOrIdentifier :: String -> TokenType
keywordOrIdentifier word =

    case word of

        "if" ->
            If

        "then" ->
            Then

        "else" ->
            Else

        "let" ->
            Let

        "in" ->
            In

        "True" ->
            TrueLiteral

        "False" ->
            FalseLiteral

        "not" ->
            Not

        "Bool" ->
            BoolType

        "UInt" ->
            UIntType

        _ ->
            Ident word

startsWith :: Char-> String-> Bool
startsWith expected input =

    case input of

        (c : _) ->
            c == expected

        [] ->
            False

advanceColumn :: SourcePos -> SourcePos
advanceColumn pos =
    pos
        { sourceColumn =
            sourceColumn pos + 1
        }

advanceColumnN :: SourcePos-> Int-> SourcePos
advanceColumnN pos amount =
    pos
        { sourceColumn =
            sourceColumn pos + amount
        }


newLine :: SourcePos -> SourcePos
newLine pos =
    SourcePos
        { sourceLine =
            sourceLine pos + 1

        , sourceColumn =
            1
        }

lexicalError :: String-> SourcePos-> a
lexicalError message pos =

    error $
        "Lexical error at line "
        ++ show (sourceLine pos)
        ++ ", column "
        ++ show (sourceColumn pos)
        ++ ": "
        ++ message

