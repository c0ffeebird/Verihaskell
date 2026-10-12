module AST
    ( Program(..)
    , Function(..)
    , Parameter(..)
    , Type(..)
    , Expr(..)
    ) where


data Program
    = Program [Function]
    deriving (Show, Eq)


data Function
    = Function
        String
        [Parameter]
        Type
        Expr
    deriving (Show, Eq)


data Parameter
    = Parameter String
    deriving (Show, Eq)


data Type
    = BoolType
    | UIntType Int
    | FunctionType Type Type
    deriving (Show, Eq)


data Expr
    = Var String
    | BoolLiteral Bool
    | UIntLiteral Integer

    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

    | Equal Expr Expr
    | NotEqual Expr Expr
    | Less Expr Expr
    | LessEqual Expr Expr
    | Greater Expr Expr
    | GreaterEqual Expr Expr

    | And Expr Expr
    | Or Expr Expr
    | Not Expr

    | If Expr Expr Expr

    | Let String Expr Expr

    | Call Expr [Expr]

    deriving (Show, Eq)
