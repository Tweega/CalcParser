// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open Parser.CalcParser

// let simple = testParseExpression("1 + 2")
// let hh = simple([123;321])
// let nestedBrackets = testParseExpression("(1 + (2 * 3))")

// printfn "%A" hh

let jollyNested = Parser.CalcParser.testParseExpression("5 * ((1 + 2) * (3 + 4))")
//jollyNested([1;2;3;4;5])