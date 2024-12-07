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
let resolveValues(values) = 
    values |> 
    List.map(fun i -> 
        ResolvedValue.Numeric (NumericValue.Float64 ((float) i))
    )
let parseResult = Parser.CalcParser.testParseExpression(expr3)

match parseResult with 
| Ok (values, _executor) -> 
    printfn "%A" values
| Error msg ->
    printfn "%s" msg

// printfn "%f" ans

// let s = "tagTot('CDT158') + tagAvg('Sinusoid', " + quote("*-1d") + ", " + quote("*") + ")"
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
