module DataAbstract.Differentiation.Support.Expr where

newtype Variable = Name String deriving (Eq)

instance Show Variable where
  show (Name a) = a

data Operator = Add | Multiply | Exponent

instance Show Operator where
  show Add = " + "
  show Multiply = " * "
  show Exponent = " ** "

data Expression a
  = Val a
  | Var Variable
  | Expr
      { op :: Operator,
        lhs :: Expression a,
        rhs :: Expression a
      }

instance (Show a) => Show (Expression a) where
  show (Val v) = show v
  show (Var v) = show v
  show (Expr op lhs rhs) = "(" ++ show lhs ++ show op ++ show rhs ++ ")"