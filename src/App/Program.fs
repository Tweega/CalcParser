// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open Parser.CalcParser

// let simple = testParseExpression("1 + 2")
// let hh = simple([123;321])
// let nestedBrackets = testParseExpression("(1 + (2 * 3))")

// printfn "%A" hh
// let expr = "5 * ((1 + 2) * (3 + 4))"
let expr1 = "1 - 2 - 3 * 4" // this gives me unecessary rhs parentheses 1 - 2 - {3 * 4}
let expr2 = "(1 - 2) - 3 - 4" // this gives me unecessary rhs parentheses 1 - 2 - {3 * 4}
let expr = "3 + 'cdt158' / 'SinusoidU'"

// let jollyNested = Parser.CalcParser.testParseExpression(expr)
// let ans = jollyNested([1;2;3;4;5])
// printfn "%f" ans

match parseExpression(expr) with 
| Ok( t, _dt)  -> 
    let hh = Parser.CalcParser.expressionFromTerm(t)
    printfn "%s" hh
| Error e ->
    printfn "%s" e