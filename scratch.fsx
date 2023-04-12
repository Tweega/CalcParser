open System.Text.RegularExpressions

type Precedence = int

type Symbol = 
| Plus
| Minus
| Multiply
| Divide
| NoOp
| Power
| Modulo

type BinaryOperator = Symbol * Precedence

type DataType = 
| Numeric
| String
| Unknown

// | Boolean?

let noOp = NoOp, 0
let opPlus = Plus, 1
let opMinus = Minus, 1
let opModulo = Modulo, 1 
let opMultiply = Multiply, 2
let opDivide = Divide, 2
let opPower = Power, 3

type Constant = 
| StringConst of string
| NumericalConst of string

type BinaryOp = {
    Operator: BinaryOperator;
    LHS: option<TypedTerm>;
    RHS: option<TypedTerm>;
}

// brackets, mult div,  plus, minus

and Value = // values are indivisible and evaluate to a base type such as int
| Tag of string // for the moment assume that tag type is always float
| Constant of Constant
| Path of string
| BinaryOpValue of BinaryOp // for bracketed expressions
| Function of string * list<TypedTerm> // labelled bracketed expression

and Term = 
| Value of Value
| BinaryOp of BinaryOp  // a binaryOp is a monoid and combines two things of the same type

and TypedTerm = Term * DataType

let makeRootOp() =
    {
        Operator = noOp;
        LHS = None;
        RHS = None;
    }


module CalcParser =
    
    type ParseResult = 
    | ParseOK of option<string> * string //text matching re, remaining string to parse, accumulator
    | ParseError of string

    type Unary = 
        | Unary of BinaryOperator
        static member Combine(unaryA: Unary, unaryB: Unary) = 
            match (unaryA, unaryB) with
            | Unary (Plus, prec), Unary (Plus, _) -> Ok (Unary (Plus, prec))
            | Unary (Minus, prec), Unary (Minus, _) -> Ok (Unary (Plus, prec))
            | Unary (Plus, prec), Unary (Minus, _) -> Ok (Unary (Minus, prec))
            | Unary (Minus, prec), Unary (Plus, _) -> Ok (Unary (Minus, prec))
            | _ -> Error "Only Plus and Minus accepted as unary operators"
    
    type Expecting  =
    | BinOp
    | Val of Unary

    type IntegralPart = int
    type FractionPart = int

    [RequireQualifiedAccessAttribute]
    type TermType =
        | Float of IntegralPart * FractionPart
        | String of string

    let quot = '\u0022'
    
    let composeParsers(f1: string -> Result<Option<TypedTerm> * string, string>) (f2: string -> Result<option<TypedTerm> * string, string>) =
        // composeParsers strings parsers together but is equivalent to oneOf in that it returns after the first successful parse
        fun(inputStr: string)  ->
            match f1(inputStr) with
            | Ok (maybeTerm, remaining) ->
                match maybeTerm with 
                | Some _x ->
                    Ok (maybeTerm, remaining)
                | None ->
                    f2(inputStr)                    
            | Error msg -> Error msg
            

    let reApply(re: string, s: string) =
        // s is a string to be parsed and it is expected that this operation will match some or none characters from the front
        // either as a direct match or as a single group in which case some marker characters, such as brackets will be thrown away
        
        let rx = Regex(re, RegexOptions.IgnoreCase + RegexOptions.Multiline +  RegexOptions.Compiled)
        let m = rx.Match(s)

        match m.Success with 
        | true -> 
            let (matchResult, newS) = 
                match m.Captures.Count with
                | 1 -> 
                    (Ok (Some m.Groups[1].Value), s[m.Length ..])
                | _ -> 
                    let msg = sprintf "More than one group matched in reg exp: %s on string: %s" re s
                    (Error msg), s

            matchResult, newS    

        | false -> 
            // printfn "no match: %s :%s " re s
            Ok None, s

    // may not use whitespace - may strip it all at the start
    let parseWhitespace(s: string)  =
        let reWhitespace: string = @"\s+"
        let newValueResult, remaining = reApply(reWhitespace, s)  
        match newValueResult with 
        | Ok maybeNewValue -> 
            ParseOK (maybeNewValue, remaining)
        | Error err ->
            ParseError err

    let parseNumber(s: string) =
        let reNumber: string = @"^\s*([0-9\.]+)"
        let newValueResult, remaining = reApply(reNumber, s)  
        match newValueResult with 
        | Ok maybeNewValue -> 
            // check that we don't have more than one decimal point here
            match maybeNewValue with 
            | Some numStr ->
                let rePoint = @"(\.)"
                match reApply(rePoint,  numStr) with //expect error if more than one match
                | Ok _maybeStr, _s -> ParseOK (maybeNewValue, remaining)
                | _ ->
                    let msg = sprintf "2 decimal points in the same number is not allowed in: %s in %s" numStr s
                    ParseError msg
            | None -> 
                // printfn "parse number failed on %s" s
                ParseOK (None, s)
        
        | Error err ->
            ParseError err


    let parseTag(s:string) =
        let reTag: string = @"^\s*\'(.+?)\'"    //tags are strings enclosed in single quotes.  same as for attrib path for pipe character
        let newValueResult, remaining = reApply(reTag, s)      
        match newValueResult with 
        | Ok maybeTag -> 
            match maybeTag with 
            | None -> 
                ParseOK (None, s)
            | Some tag ->
                match tag with 
                | "" ->
                    let msg = "Error: Empty tag name"
                    ParseError msg
                | _ ->
                    let illegalChars = sprintf "%s%c" @"*'\?;{}[\]\|\\`\" quot //we wouldn't actually detect single quote here
                    let reIllegal = sprintf "[%s]" illegalChars
                    match reApply(reIllegal, tag) with 
                    | Ok (Some x),_s -> 
                        let msg = sprintf "Illegal character (%s)in tag name: %s" x s
                        ParseError msg
                    | _ ->
                        let reNonPrintable = @"[-~]"
                        match reApply(reNonPrintable, tag) with 
                        | Ok (Some _), _s -> 
                            let msg = sprintf "Non printable character in tag name: %s" s
                            ParseError msg
                        | _ ->
                            ParseOK (maybeTag, remaining)
                    
        | Error msg -> ParseError msg
        

    let parseString(s:string) =
        let reString: string = sprintf @"^\s*\%c(.+)\%c" quot quot // strings are enclosed in double quotes
        let newValueResult, remaining = reApply(reString, s)      
        match newValueResult with 
        | Ok maybeStr -> 
        
            match maybeStr with 
            | None -> ParseOK (None, s)

            | Some str ->
                let reNonPrintable = @"[-~]"
                match reApply(reNonPrintable, str) with 
                | Ok (Some _), _s -> 
                    let msg = sprintf "Non printable character in string: %s" s
                    ParseError msg
                | _ ->
                    ParseOK (maybeStr, remaining)
                    
        | Error msg -> ParseError msg
        
            
    let parseFunctionName(s:string) =
        let reString: string = sprintf @"^\s*([A-Za-z][A-Za-z0-9]*)\s*\(" //function name starts with alpha optionally continues with alphaNum and terminates with open parenthesis
        let newValueResult, remaining = reApply(reString, s)      
        match newValueResult with 
        | Ok maybeFuncName -> 
        
            match maybeFuncName with 
            | None ->ParseOK (None, s)

            | Some str ->
                let reNonPrintable = @"[-~]"
                match reApply(reNonPrintable, str) with 
                | Ok (Some _), _s -> 
                    let msg = sprintf "Non printable character in string: %s" s
                    ParseError msg
                | _ ->
                    ParseOK (maybeFuncName, remaining)
                    
        | Error msg -> ParseError msg
        
        
    let parseBrackets(s:string) =
        let reParens: string = sprintf @"^\s*\((.*?)\)" //function name starts with alpha optionally continues with alphaNum and terminates with open parenthesis
        let newValueResult, remaining = reApply(reParens, s)      
        match newValueResult with 
        | Ok maybeTerm -> 
        
            match maybeTerm with 
            | None -> ParseOK (None, s)

            | Some str ->
                printfn "Remaining for brackets: %s" remaining
                let reNonPrintable = @"[-~]"
                match reApply(reNonPrintable, str) with 
                | Ok (Some _), _s -> 
                    let msg = sprintf "Non printable character in bracketed expression: %s" s
                    ParseError msg
                | _ ->
                    ParseOK (maybeTerm, remaining)
                    
        | Error msg -> ParseError msg
        
    

    let parseOperator(s:string) =
        let reOp = @"^\s*([+-\/\*\^%])"
        let newValueResult, remaining = reApply(reOp, s)      
        match newValueResult with 
        | Ok maybeOperator ->
            ParseOK (maybeOperator, remaining)
        | Error msg -> ParseError msg


    let parseAndHandleString(input: string) =
        match parseString(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> (StringConst >> Constant >> Value)
                
                Ok (Some (term, DataType.String), remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    let parseAndHandleNumber(input: string) =
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> (NumericalConst >> Constant >> Value)
                Ok (Some (term, DataType.Numeric), remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    let parseAndHandleTag(input: string) =
        match parseTag(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Tag >> Value))
                Ok (Some (term, DataType.Numeric), remaining) // assume tags emit numeric values for the moment
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    let parseAndHandlePath(input: string, terms: list<TypedTerm>) =
        match parseTag(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Path >> Value))
                Ok (Some (term, DataType.Numeric), remaining) // assume that attributes return numeric values for the moment
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    
    let parseAndHandleBinaryOperator(input: string) =
        match parseOperator(input) with 

        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some opStr ->
                // printfn "OP: %s" opStr
                let binOp = 
                    match opStr with 
                    | "+" -> opPlus
                    | "-" -> opMinus
                    | "*" -> opMultiply
                    | "/" -> opDivide
                    | "%" -> opModulo
                    | "^" -> opPower
                    | _ -> noOp

                let binOp' = {
                    Operator = binOp;
                    LHS = None;
                    RHS = None;
                } 
                
                let term = binOp' |> BinaryOp

                Ok (Some (term, DataType.Unknown), remaining)

            | None ->
                Ok (None, input)

        | ParseError msg ->
            Error msg

    let (>=>) = composeParsers

    let rec parseExpression(expr: string) =
        let rootOp = makeRootOp()
        // let opStack: Stack<BinaryOp> = Stack []
        // let rootStack = Stack.push rootOp opStack

        let parseAndHandleValue = parseAndHandleTag >=> parseAndHandleNumber >=> parseAndHandleString >=> parseAndHandleBrackets
        let parseAndHandleTerm = parseAndHandleValue >=> parseAndHandleBinaryOperator >=> parseAndHandleFunction

        let rec mergeOpVals (operators: list<BinaryOp>, values:list<Value * DataType>, acc:list<BinaryOp>) =
            match (operators, values) with
            | [], [] -> 
                //this would be an error
                let msg =  "Same number of ops as values"
                Error msg
            | [], (hVal, hDt) :: [] -> 
                // there should be one more value than operator, and this is the last one, which goes on lhs of the accumulator head
                match acc with 
                | [] ->
                    // we only have a term in this expression so return that
                    Ok ((Value  hVal), hDt)
                | hAcc :: tAcc ->
                    let rec mergeOps(lhsOp: BinaryOp, rhsOp: BinaryOp) =
                        printfn "mergeOps"
                        match lhsOp.RHS with 
                        | None ->
                            // this would be an error with the program and should not happen
                            let msg = sprintf "Error BinaryOp without RHS"
                            Error msg
                        | Some (rhsTerm, rhsDt) ->
                            match rhsTerm with
                            | Value _v ->
                                // compare precedences  -we could just look these up when we need to tk
                                let (_rhsSym, rhsPrec) = rhsOp.Operator
                                let (_lhsSym, lhsPrec) = lhsOp.Operator
                                
                                match rhsPrec > lhsPrec with 
                                | true -> 
                                    printfn "true"

                                    // lhs.rhs moves to rhs.lhs and this new binOp becomes lhs.rhs
                                    let rhsOp' = { rhsOp with LHS = lhsOp.RHS } 
                                    let rhsTypedTerm =  (rhsOp' |> BinaryOp, rhsDt)
                                    // let jj = { lhsOp with RHS =  Some rhsTypedTerm }
                                    // printfn "JJ:%A" jj
                                    { lhsOp with RHS =  Some rhsTypedTerm } |> Ok

                                | false ->
                                    printfn "false"
                                    // no precedence conflict so rhs becomes new head op with lhs set to current head 
                                    // taking data type from lhs - type validation should happen before this merge - an expression should only contain one type
                                    let lhsTypedTerm =  (lhsOp |> BinaryOp, rhsDt)
                                    { rhsOp with LHS = Some lhsTypedTerm } |> Ok
                                
                            | BinaryOp bop ->
                                printfn "We should not  be here yet"
                                let mergedOp = mergeOps(bop, rhsOp)
                                printfn "Merged BOP:%A" mergedOp
                                match mergedOp with 
                                | Ok bop' ->
                                    // put bop' into lhsOp.RHS
                                    let rhsTypedTerm =  ((bop' |> BinaryOp), rhsDt)
                                    { lhsOp with RHS = Some rhsTypedTerm } |> Ok

                                | Error msg -> Error msg
                                
                    // put last value into lhs of head of accumulator, if there is one
                    let op = { hAcc with LHS = Some ((Value hVal), hDt) }
                    // let acc' = op :: tAcc
                    let mergeRes = 
                        tAcc 
                        |> List.fold(fun exprRes rhs ->
                            match exprRes with 
                            | Ok bOp' -> 
                                mergeOps(bOp', rhs)                            
                            | Error msg -> Error msg
                        ) (Ok op)
            
                    match mergeRes with 
                    | Ok bop' ->
                        // printfn "Success: %A" bop'
                        Ok ((bop' |> BinaryOp), hDt)
                    | Error msg -> 
                        // printfn "Failure: %s" msg
                        Error msg
            
            | hOp :: tOp, (hVal, hDt) :: tVal -> 
                //  the accumulator is a list of operators with rhs set to value

                let op = { hOp with RHS = Some ((Value hVal), hDt) }

                mergeOpVals(tOp, tVal, op :: acc)
            | _, _ -> Error "unexpected error in mergeOpVals"

            
        let buildAST(binOps: list<BinaryOp>, values: list<Value * DataType>) =
            //pair up values and operators - there should be one more value than operator
            // let rec dodah()
            mergeOpVals(binOps, values, [])
            
        let rec gatherTerms(input: string, values: list<Value * DataType>, binOps: list<BinaryOp>, expecting: Expecting) : Result<list<Value * DataType> * list<BinaryOp>, string> =    
            // this function does an initial pass through the expression, creating a list of terms
            printfn "Gathering terms for: %s" input
            match input with 
            | "" -> // we have finished parsing the input string
                Ok (values, binOps)
            | remaining -> // more to parse
                let res = parseAndHandleTerm(remaining)
                match res with 
                | Ok (maybeTerm, remaining') -> 
                    match maybeTerm with 
                    | Some (term, dt) -> 
                        match expecting with 
                        | BinOp ->
                            match term with 
                            | Value _v -> 
                                let msg = "Expecting Binary Operator, but got value"
                                Error msg
                            | BinaryOp binOp ->
                                // we have got what we were expecting - add this operator to binOps list
                                // printfn "Adding bin op to list %A" binOp
                                let binOps' = binOp :: binOps
                                gatherTerms(remaining', values, binOps', Expecting.Val (Unary opPlus))
                        | Val unary ->
                            match term with 
                            | BinaryOp bop ->
                                // treat this as a unary operator
                                // printfn "Treating as unary!"
                                match Unary.Combine(unary, Unary bop.Operator) with 
                                | Ok unary' -> 
                                    gatherTerms(remaining', values, binOps, Expecting.Val unary')    
                                | Error msg -> Error msg
                            | Value v ->
                                match unary with 
                                | Unary (Plus, _) ->
                                    // add the value as is to values list
                                    let values' = (v, dt) :: values
                                    gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                | Unary (Minus, _) ->
                                    // multiply value by -1 if it is a constant number 
                                    // check  that dt is Numeric otherwose error
                                    match v with 
                                    | Constant (NumericalConst nStr) ->     
                                        let maybeN =                                    
                                            match System.Double.TryParse(nStr) with 
                                            | true, n ->  Some ((string) (n * -1.0))
                                            | _ -> 
                                                // we should probably throw an error here tk
                                                None
                                        match maybeN with 
                                        | Some n' ->
                                            let v' = n' |> (NumericalConst >> Constant)

                                            let values' = (v', dt) :: values
                                            gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                        | None -> 
                                            Error "Unable to parse float constant"
                                    | _->
                                        // we need to handle other numeric types such as tag here tk
                                        let minusOneOp = (string) -1 |> (NumericalConst >> Constant)
                                        let lhs' = (Value minusOneOp, DataType.Numeric) |> Some

                                        let op = {
                                            Operator = opMultiply;
                                            LHS = lhs';
                                            RHS = Some (term, DataType.Numeric);
                                        }
                                        let bopVal = BinaryOpValue op
                                        let values' = (bopVal, dt) :: values
                                        gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                | _ -> Error "Only Plus and Minus can be used as unary operators"


                    | None -> 
                        let msg = sprintf "Unable to parse expression %s after %s, remaining:%s" expr input remaining
                        Error msg
                | Error msg -> 
                    Error msg

        let result = gatherTerms(expr, [], [], Expecting.Val (Unary opPlus))


        match result with 
        | Ok (typedValues, binaryOps) ->
            // the initial term should be a value
            // now process the lists into a calc tree
            let astRes = buildAST(binaryOps, typedValues)
            // printfn "%A" astRes
            astRes

        | Error msg ->
            Error msg

    and parseAndHandleBrackets(input: string) =
        match parseBrackets(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let termRes = parseExpression(str)
                match termRes with 
                | Ok (term, dt) ->
                    // make a  value out of this term if it is not already one
                    let t = 
                        match term with  
                        | Value _v -> term
                        | BinaryOp bop -> 
                            Value (BinaryOpValue bop)
                    Ok (Some (t, dt), remaining)
                | Error msg -> Error msg
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    and parseAndHandleFunction(input: string) =
        match parseFunctionName(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            let hh = parseBrackets(remaining)
            match hh with 
            | ParseOK (maybeMatch', remaining') -> 
                match maybeMatch with 
                | Some parameters -> 
                    // let expressions = parameters.Split ','
                    // create a root binary operator for each parameter

                    // let results = 
                    //     expressions |> 
                    //     List.ofArray |>
                    //     List.fold(fun acc input' -> 
                    //         // we could in theory run these in parallel by creating a list of unit functions
                    //         // let result' = parseExpression(input')
                    //         // result' :: acc
                    //         acc
                    //     ) List<Result<BinaryOp,string>>.Empty
                    
                    // now run through the results of each paramter from list<results>
                    // let jj =
                    //     results |>
                    //     List.fold(acc jj ->
                    //         acc
                    //     ) Ok ()

                        // Ok (Some (term, DataType.Numeric), remaining) // assume that attributes return numeric values for the moment
                    Ok (None, input)

                 | None -> 
           
                    // let jj = "12.2" |> (NumericalConst >> Constant >> Value)
                    // Ok (Some jj, DataType.String, remaining)
                    Ok (None, input)

            | ParseError msg -> Error msg

        | ParseError msg ->
            Error msg
        
    
  //> CalcParser.parseExpression("1 + 2 * 3  ^ 4")
  //> CalcParser.parseExpression("1 * (2 + 3)")
  //> CalcParser.parseExpression("(2 + 3) * 4")
  //> CalcParser.parseExpression("1 + (4 * 5)")
  //> CalcParser.parseExpression("(1 + 2) + (4 * 5)")
  //> CalcParser.parseExpression("(1 + 2) + (4 * 5) / 6")
  //> CalcParser.parseExpression("1 * - 1")
  //> CalcParser.parseExpression("3 * - 'Sinusoid'")
  //> CalcParser.parseExpression("'Sinusoid' * 'CDT158'")

