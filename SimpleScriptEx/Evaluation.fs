module Evaluation

open Types
open System

let eval (expr:Expression) =
    let defaultValueMap = 
        ValueMap([("pi", Number(Math.PI))
                  ("e", Number(Math.E))
                  ("newLine", Value.String("\n"))])
    let addGlobalFunctions valueMap =
        match expr with
        | ExprList(exprList) ->
            let rec gatherFunctions exprList (funcMap:ValueMap) = 
                match exprList with
                | head :: tail ->
                    match head with
                    | FuncDecl(n, p, e) ->
                        gatherFunctions tail (funcMap.Add(n, Function(p, e)))
                    | _ ->
                        gatherFunctions tail funcMap
                 | [] -> funcMap
            gatherFunctions exprList (valueMap)
        | _ -> valueMap
    let initialValueMap = defaultValueMap |> addGlobalFunctions
    let initialScope = [initialValueMap]

    let rec recEval (scope:ValueMap list) (value:Value) (expr:Expression)=
        let builtinFunctions =
            Map([("Print", fun (a: Value list) -> 
                    a |> List.iter(
                        function
                        | String(s) -> printf "%s" s
                        | Number(n) -> printf "%f" n
                        | x -> printf "%A" x); Unit)
                 ("Invoke", fun (a: Value list) ->
                    match a.Head with
                    | Function(p, e) ->
                        let scope = [ValueMap(List.zip p a.Tail)]
                        recEval scope Unit e
                    | _ -> ErrorValue)])

        let rec findVarInScope name (scope: ValueMap list) =
            match scope with
            | head :: tail ->
                let valueOption = head.TryFind(name)
                match valueOption with
                | Some(v) -> v
                | None -> findVarInScope name scope.Tail
            | [] -> ErrorValue

        let runFunc fName fArgs scope =
            let values = fArgs |> List.map(recEval scope value)
            let rec findInScope fName (scope:ValueMap list) =
                match scope with
                | head :: tail ->
                    let valueOption = head.TryFind(fName)
                    match valueOption with
                    | Some(v) ->
                        match v with
                        | Function(p, e) ->
                            let argMap = ValueMap(List.zip p values)
                            let funcScope = argMap :: scope
                            recEval funcScope Unit e
                        | _ -> ErrorValue
                    | None -> findInScope fName tail
                | [] -> 
                    let valueOption = builtinFunctions.TryFind(fName)
                    match valueOption with
                    | Some(func) -> func values
                    | None -> ErrorValue
            findInScope fName scope
             
        let resolve lhs rhs =
            let lhsv = recEval scope value lhs
            match lhsv with
            | Object(o) ->
                match rhs with
                | Variable(v) ->
                    o |> List.find(fun n -> (fst n) = v) |> snd
                | _ -> ErrorValue
            | Array(a) -> 
                let rhsv = recEval scope value rhs
                match rhsv with
                | Number(n) ->
                    a.[int(n)]
                | _ -> ErrorValue
            | _ -> ErrorValue

        let compute lhs rhs op =
            let calc lhs rhs opf =
                match (lhs, rhs) with
                | (Number(l), Number(r)) -> Number(opf l r)
                | _ -> ErrorValue
            let compf lhs rhs opf =
                match (lhs, rhs) with
                | (Number(l), Number(r)) -> Bool(opf l r)
                | _ -> ErrorValue

            let comb lhs rhs opb =
                match (lhs, rhs) with
                | (Bool(l), Bool(r)) -> Bool(opb l r)
                | _ -> ErrorValue

            match op with
            | Addition ->
                match lhs with
                | Number(_) -> calc lhs rhs (+)
                | String(l) ->
                    match rhs with
                    | String(r) -> Value.String(l + r)
                    | _ -> ErrorValue
                | _ -> ErrorValue
            | Subtraction -> calc lhs rhs (-)
            | Multiplication -> calc lhs rhs (*) (* *)
            | Division -> calc lhs rhs (/)
            | LessThan -> compf lhs rhs (<)
            | LessOrEqual -> compf lhs rhs (<=)
            | Equality -> compf lhs rhs (=)
            | GreaterOrEqual -> compf lhs rhs (>=)
            | GreaterThan -> compf lhs rhs (>)
            | And -> comb lhs rhs (&&)
            | Or -> comb lhs rhs (||)
            | _ -> ErrorValue

        match expr with
        | ExprList(exprList) ->
            let rec evalList exprTail scope value =
                match exprTail with
                | head :: tail ->
                    match head with
                    | Declaration(n, e) ->
                        let v = recEval scope value e
                        evalList tail (ValueMap[(n, v)] :: scope) v
                    | _ -> evalList tail scope (recEval scope value head)
                | [] -> value
            evalList exprList scope value
        | BinOp(op, lhs, rhs) ->
            match op with
            | Dot | At -> resolve lhs rhs
            | _ ->
                let lhsv = recEval scope value lhs
                let rhsv = recEval scope value rhs
                compute lhsv rhsv op
        | Branch(cond, tr, fl) ->
            let v = recEval scope value cond
            match v with
            | Bool(b) ->
                if b then
                    recEval scope value tr
                else
                    match fl with
                    | Some(expr) ->
                        recEval scope value expr
                    | None -> Unit
            | _ -> ErrorValue
        | ArrayInit(exprList) ->
            exprList |> List.map(recEval scope value) |> Value.Array
        | Variable(var) ->
            findVarInScope var scope
        | Constant(c) ->
            match c with
            | Reference(r) -> findVarInScope r scope
            | _ -> c  
        | FuncCall(n, e) -> runFunc n e scope
        | _ -> ErrorValue
    recEval initialScope Unit expr