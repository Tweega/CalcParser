#r "src/Parser/bin/Debug/net6.0/Parser.dll"

let quot = (string) '\u0022'    
let quote(s:string) = 
  quot + s + quot

let simple = Parser.CalcParser.testParseExpression("1 + 2")
simple([123;321])
let moreComplex = Parser.CalcParser.testParseExpression("1 + 2 * 3")
moreComplex([2;3;1])
  

let withBracket = Parser.CalcParser.testParseExpression("1 * (2 + 3)")
withBracket([2;3;1])
  

let moreComplexEval = Parser.CalcParser.testParseExpression("(1 + 2) + (3 * 4) * 5")
moreComplexEval([1;2;3;4;5])
moreComplexEval([21;2;4;7;13])

let nestedBrackets = Parser.CalcParser.testParseExpression("(1 + (2 * 3))")
nestedBrackets([2;3;1])
nestedBrackets([4;5;6])

let jollyNested = Parser.CalcParser.testParseExpression("5 * ((1 + 2) * (3 + 4))")
jollyNested([1;2;3;4;5])
jollyNested([11;21;23;14;5])

let superNested = Parser.CalcParser.testParseExpression("(105 / (5 * ((1 + 2) * (3 + 4))))")
superNested([1;2;3;4;5;105])

let s = "tagTot('CDT158') + tagAvg('Sinusoid', " + quote("*-1d") + ", " + quote("*") + ")"
let res = Parser.CalcParser.parseExpression(s)
match res with 
| Ok (t, _dt) -> 
  let expr = Parser.CalcParser.expressionFromTerm(t)
  printfn "%s" expr
| Error err -> 
  printfn "%s" err
  //> let expr = Parser.CalcParser.parseExpression("1 + 2")

  // Parser.CalcParser.createCalcDef(expr)
  // let plusEval = Parser.CalcParser.testParseExpression("1 + 2")
