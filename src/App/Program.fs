// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open Parser.CalcParser
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
let expr3 = "3 + 'cdt158' / 'SinusoidU'"

// let jollyNested = Parser.CalcParser.testParseExpression(expr)
// let ans = jollyNested([1;2;3;4;5])
// printfn "%f" ans

// let s = "tagTot('CDT158') + tagAvg('Sinusoid', " + quote("*-1d") + ", " + quote("*") + ")"
let s = "'CDT158' / 'Sinusoid' * 100"
//--------------------
// let yy = Parser.CalcParser.parseExpression(s)

// match parseExpression(s) with 
// | Ok( t, _dt)  -> 
//     // let hh = Parser.CalcParser.expressionFromTerm(t)
//     let hh = Parser.CalcParser.AFAnalysisFromTerm(t)
//     printfn "%s" hh
// | Error e ->
//     printfn "%s" e
//-----------------------------

let xPath = "./pump"
let hh = Parser.JutzParser.parseExpression(xPath)
printfn "%A" hh
ignore <| System.Console.ReadLine()
