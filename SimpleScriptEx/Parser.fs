module Parser

open FParsec
open Types

type Parser<'a> = Parser<'a, unit>

let ws = spaces
let str x = pstring x .>> ws

let identifier =
    let firstChar c = c = '_' || isLetter c
    let others c = firstChar c || isDigit c
    many1Satisfy2L firstChar others "identifier" .>> ws

let opp = OperatorPrecedenceParser<Expression, _, _>()
opp.AddOperator(InfixOperator("+", ws, 20, Associativity.Left, fun lhs rhs -> BinOp(Addition, lhs, rhs)))
opp.AddOperator(InfixOperator("-", ws, 20, Associativity.Left, fun lhs rhs -> BinOp(Subtraction, lhs, rhs)))
opp.AddOperator(InfixOperator("*", ws, 30, Associativity.Left, fun lhs rhs -> BinOp(Multiplication, lhs, rhs)))
opp.AddOperator(InfixOperator("/", ws, 30, Associativity.Left, fun lhs rhs -> BinOp(Division, lhs, rhs)))
opp.AddOperator(InfixOperator("==", ws, 10, Associativity.Left, fun lhs rhs -> BinOp(Equality, lhs, rhs)))
opp.AddOperator(InfixOperator("<", ws, 10, Associativity.Left, fun lhs rhs -> BinOp(LessThan, lhs, rhs)))
opp.AddOperator(InfixOperator("<=", ws, 10, Associativity.Left, fun lhs rhs -> BinOp(LessOrEqual, lhs, rhs)))
opp.AddOperator(InfixOperator(">", ws, 10, Associativity.Left, fun lhs rhs -> BinOp(GreaterThan, lhs, rhs)))
opp.AddOperator(InfixOperator(">=", ws, 10, Associativity.Left, fun lhs rhs -> BinOp(GreaterOrEqual, lhs, rhs)))
opp.AddOperator(InfixOperator("and", ws, 5, Associativity.Left, fun lhs rhs -> BinOp(And, lhs, rhs)))
opp.AddOperator(InfixOperator("or", ws, 6, Associativity.Left, fun lhs rhs -> BinOp(Or, lhs, rhs)))

let expr = opp.ExpressionParser
let exprList = sepEndBy expr (str ";") |> between (str "{") (str "}") |>> ExprList
let branch, branchRef = createParserForwardedToRef<Expression, _>()
do branchRef :=
    pipe3 (str "if" >>. expr)
          (expr)
          (opt(str "else" >>. choice[branch; expr]))
          (fun cond tr fl -> Branch(cond, tr, fl))
let variable = identifier .>> ws |>> Variable
let paramList = (sepEndBy identifier (str ";")) |> between (str "(") (str ")")
let funcDecl = 
    attempt(
        pipe3 (str "fn" >>. identifier)
              (paramList)
              (expr)
              (fun n p e -> FuncDecl(n, p, e)))
let funcProto =
    attempt(pipe2 (str "fn" >>. paramList)
          (expr)
          (fun p e -> Function(p, e) |> Constant))
let funcCall = attempt(identifier .>>. ((sepEndBy expr (str ";")) |> between (str "(") (str ")"))) |>> FuncCall
let numberConstant = pfloat .>> ws |>> Number
let stringConstant = manySatisfy(fun c -> c <> '"') |> between (pstring "\"") (str "\"") |>> string |>> String
let boolConstant =
    choice[str "true"
           str "false"] |>> function
                            | "true" -> Bool(true)
                            | _ -> Bool(false)
let constant =
    choice[numberConstant
           stringConstant
           boolConstant] |>> Constant

let declaration = ((str "let " >>. identifier) .>>. (str "=" >>. expr)) |>> Declaration
opp.TermParser <-
    choice[branch
           funcDecl
           funcProto
           declaration
           funcCall
           variable
           constant
           exprList
           expr |> between (str "(") (str ")")]

let script:Parser<_> = ws >>. (sepEndBy expr (str ";")) |>> ExprList .>> eof