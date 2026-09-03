module Lexer where

import Data.Char (isDigit, isAlpha, isAlphaNum)

-- 1. Definição dos tipos de tokens que a nossa linguagem suporta
data TokenType 
  = Let             -- Palavra-chave 'let'
  | Print           -- Palavra-chave 'print'
  | IntLit Int      -- Literais inteiros (ex: 10, 42)
  | Ident String    -- Identificadores / nomes de variáveis (ex: "x", "total")
  | Plus            -- Operador '+'
  | Star            -- Operador '*'
  | Equal           -- Operador '='
  | Semicolon       -- Ponto e vírgula ';'
  deriving (Show, Eq)

-- 2. Estrutura do Token com rastreamento de linha para facilitar o debug
data Token = Token
  { tokenType :: TokenType
  , tokenLine :: Int
  } deriving (Show, Eq)

-- 3. Função principal que inicia o processo na linha 1
tokenize :: String -> [Token]
tokenize source = go source 1
  where
    -- Caso base: fim da string
    go [] _ = []
    
    -- Ignora espaços, tabs e retornos de carro
    go (c:cs) line
      | c `elem` " \t\r" = go cs line
      | c == '\n'       = go cs (line + 1)
      
      -- Símbolos de um caractere
      | c == '+'        = Token Plus line : go cs line
      | c == '*'        = Token Star line : go cs line
      | c == '='        = Token Equal line : go cs line
      | c == ';'        = Token Semicolon line : go cs line
      
      -- Leitura de números inteiros
      | isDigit c       = 
          let (numStr, rest) = span isDigit (c:cs)
          in Token (IntLit (read numStr)) line : go rest line
          
      -- Leitura de identificadores e palavras-chave
      | isAlpha c       = 
          let (word, rest) = span isAlphaNum (c:cs)
              tokType = case word of
                          "let"   -> Let
                          "print" -> Print
                          _       -> Ident word
          in Token tokType line : go rest line
          
      -- Qualquer caractere não reconhecido gera erro imediato com a linha
      | otherwise       = error $ "Erro Léxico: Caractere inválido '" ++ [c] ++ "' na linha " ++ show line

      -- TODO: incrementar implementação para casos futuros
      