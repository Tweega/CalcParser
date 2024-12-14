// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open Parser.CalcParser
open Parser.ParserTypes
open Parser.JutzParser

// let simple = testParseExpression("1 + 2")
// let hh = simple([123;321])
// let nestedBrackets = testParseExpression("(1 + (2 * 3))")
let quote(s:string) =
    let sQuote = (string) quot
    (string) sQuote + s + sQuote

// printfn "%A" hh
let expr = "5 * ((1 + 2) * (3 + 4))"
let expr1 = "1 - 2 - 3 * 4" // this gives me unecessary rhs parentheses 1 - 2 - {3 * 4}
let expr2 = "(1 - 2) - 3 - 4" // this gives me unecessary rhs parentheses 1 - 2 - {3 * 4}
let expr3 = "3 + 'CDT158' / 'Sinusoid'"

let resolveFloats(values) = 
    values |> 
    List.map(fun i -> 
        ResolvedValue.Numeric (NumericValue.Float64 ((float) i))
    )

(*
let rec doArgs(typedTerms: list<TypedTerm>) = 
    // transform typed terms into resolved values
    let hh = 
        typedTerms |> 
        List.fold(fun acc (term, _dt) -> 
            match term with 
            | Term.BinaryOp _bop -> 
                Error "we should not be returning binaryOps from parseExpression - only values tk"
            | Term.Value v ->
                match v with 
                | Value.
                
            acc
        ) (Ok [])

    hh
*)


let parseResult = Parser.CalcParser.testParseExpression(expr3)

// this will eventually take a start and end time, possibly a filter expression


let generateRandomFloats n =
    let random = System.Random()
    List.init n (fun _ -> random.NextDouble())

let getValues(tag: string, eventCount) = 
    generateRandomFloats eventCount

(*
match parseResult with 
| Ok (values, evaluator) -> 
    let cdtValues = 
        getValues("CDT158", 10)
        |> resolveFloats
    let sinusoidValues = 
        getValues("SINUSOID", 10)
        |> resolveFloats

    let xx = List.transpose([cdtValues; sinusoidValues])
    let hh = xx |> List.map evaluator
    
    
    // let rvs = resolveFloats([1.1; 2.2])
    // let ans = rvs |> evaluator

    // printfn "%A" values
    printfn "ans: %A" hh
| Error msg ->
    printfn "%s" msg

*)

// printfn "%f" ans

let s = "1 + tagAvg('Sinusoid', " + quote("*-1d") + ", " + quote("*") + ")"
// let rs = s |> resolveFunctions
// //--------------------
// let yy = Parser.CalcParser.parseExpression(s)

if 1 = 2 then
    // let s = "'CDT158' / 'Sinusoid' * 100"
    // match Parser.CalcParser.parseExpression(s) with 
    // | Ok( t, _dt)  -> 
    //     // let hh = Parser.CalcParser.expressionFromTerm(t)
    //     //let hh = Parser.CalcParser.AFAnalysisFromTerm(t)
    //     printfn "%s" hh
    // | Error e ->
    //     printfn "%s" e
// -----------------------------

    let xPath = "./pump[@pressure = 1][@flow = 2]"
    // let xPath = "./pump/@pressure"
    let jpTermsResult = Parser.JutzParser.parseExpression(xPath)
    match jpTermsResult with
    | Ok jpTerms ->
        printfn  "Successful parse - %A" jpTerms
        printfn "attempting compile"
        let cc = Parser.JutzParser.compileXPathFromTerms(jpTerms)
        printfn "%A" cc
    | Error err ->
        printfn "%s" err
            

ignore <| System.Console.ReadLine()
