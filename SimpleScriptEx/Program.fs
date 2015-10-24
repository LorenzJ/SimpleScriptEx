// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Types
open Evaluation
open System
open Parser
open FParsec

let source = """
fn main()
{
    let myLoop = fn(i; func)
    {
        let accumulator = fn(i; func; value)
        {
            if i > 0
            {
                accumulator(i - 1; func; value + func());
            } else value;
        };
        accumulator(i; func; 0.0);
    };
    myLoop(5; fn() { 2; });
};
main()
"""
[<EntryPoint>]
let main argv = 
    match run script source with
    | Success(ast, _, _) -> printfn "%A" (eval ast)
    | Failure(msg, _, _) -> printfn "%s" msg
    Console.ReadKey true |> ignore
    0 // return an integer exit code
