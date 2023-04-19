#r "src/Parser/bin/Debug/net6.0/Parser.dll"


let simple = Parser.CalcParser.testParseExpression("1 + 2")
simple([123;321])
let moreComplex = Parser.CalcParser.testParseExpression("1 + 2 * 3")
moreComplex([2;3;1])
  

let withBracket = Parser.CalcParser.testParseExpression("1 * (2 + 3)")
withBracket([2;3;1])
  

let moreComplexEval = Parser.CalcParser.testParseExpression("(1 + 2) + (3 * 4) * 5")
moreComplexEval([1;2;3;4;5])

// working here -does not like nested brackets - probably the reg exp that gets the insides of brackets maybe stopping at the first close
// not sure how to handle that - need to make sure that the openings match the closes.
let nestedBrackets = Parser.CalcParser.testParseExpression("(1 + (2 * 3))")
nestedBrackets([2;3;1])
  //> let moreComplexEval = Parser.CalcParser.testParseExpression("1 + 2 + 3 * 4 * 5")

let jollyNested = Parser.CalcParser.testParseExpression("5 * ((1 + 2) * (3 + 4))")
jollyNested([1;2;3;4;5])

let superNested = Parser.CalcParser.testParseExpression("(105 / (5 * ((1 + 2) * (3 + 4))))")
superNested([1;2;3;4;5;105])

  //> Parser.CalcParser.parseExpression("1 * - 1")
  //> Parser.CalcParser.parseExpression("3 * - 'Sinusoid'")
  //> Parser.CalcParser.parseExpression("'Sinusoid' * 'CDT158'")
  //> Parser.CalcParser.parseExpression("2 + tagTot(1)")
  //> Parser.CalcParser.parseExpression("tagTot(1, 2, 3, 4)")
  //> Parser.CalcParser.parseExpression("2")
  //> Parser.CalcParser.parseExpression("'Sinusoid'")
let s = "tagTot(\"CDT158\") + tagAvg(\"Sinusoid\")"
  //> Parser.CalcParser.parseExpression(s)

  //> let expr = Parser.CalcParser.parseExpression("1 + 2")

  // Parser.CalcParser.createCalcDef(expr)
  // let plusEval = Parser.CalcParser.testParseExpression("1 + 2")
