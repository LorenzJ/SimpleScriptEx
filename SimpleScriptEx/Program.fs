// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Types
open Evaluation
open System
open Parser
open FParsec

let source = """
fn pow(x; p)
{
    let recPow = fn(a; x; i)
        if i == 0 a
        else recPow(a * x; x; i - 1);
    recPow(1; x; p);
};

fn fact(x) if x == 0 1
           else x * fact(x - 1);

fn main()
{
    {
        let loop = fn(cur; end)
        {
            Print("Factorial("; cur; ") = "; fact(cur); newLine);
            if cur < end loop(cur + 1; end);
        };
        loop(0; 5);
    };
    Print("power(5, 2) = "; pow(5; 3); newLine);
    { 
        let a = [125.0; (ref pow)];
        Print(Invoke(a@1; 5; 2));
    };
    
};

fn durp() "Lol";

main();
Print(durp());
"""
[<EntryPoint>]
let main argv = 
    match run script source with
    | Success(ast, _, _) -> printfn "%A" (eval ast)
    | Failure(msg, _, _) -> printfn "%s" msg
    Console.ReadKey true |> ignore
    0 // return an integer exit code
