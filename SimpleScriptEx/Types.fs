module Types

type Operation = Addition
               | Subtraction
               | Multiplication
               | Division
               | LessThan
               | LessOrEqual
               | Equality
               | GreaterOrEqual
               | GreaterThan
               | And
               | Or
               | Not

type Value = Number   of float
           | String   of string
           | Bool     of bool
           | ErrorValue
           | Array    of Value list
           | Object   of (string * Value) list
           | Function of string list * Expression
           | Unit

and Expression = Constant    of Value
               | Variable    of string
               | Declaration of string * Expression
               | BinOp       of Operation * Expression * Expression
               | Branch      of Expression * Expression * Expression option
               | FuncDecl    of string * string list * Expression
               | FuncCall    of string * Expression list
               | ExprList    of Expression list

type ValueMap = Map<string, Value>
