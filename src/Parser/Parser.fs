namespace Parser

module CalcParser =
    open ParserTypes
    open System.Text.RegularExpressions

    
    [<RequireQualifiedAccessAttribute>]
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
        printfn "reApply has received [%s]" s
        let rx = Regex(re, RegexOptions.IgnoreCase + RegexOptions.Multiline +  RegexOptions.Compiled)
        let m = rx.Match(s)

        match m.Success with 
        | true -> 
            let (matchResult, newS) = 
                match m.Captures.Count with
                | 1 ->  // working here on whitespace issue.  we may need to match on whitespace separately
                    printfn "We have a match: %A %d" m.Captures[0].Value m.Length
                    (Ok (Some m.Groups[1].Value), s[m.Length ..])
                | _ -> 
                    let msg = sprintf "More than one group matched in reg exp: %s on string: %s" re s
                    (Error msg), s

            matchResult, newS    

        | false -> 
            // printfn "no match: %s :%s " re s
            Ok None, s

    let parseWhitespace(s: string)  =
        let reWhitespace: string = @"^\s+"
        let newValueResult, remaining = reApply(reWhitespace, s)  
        match newValueResult with 
        | Ok maybeNewValue ->
            ParseOK (maybeNewValue, remaining)
        | Error err ->
            ParseError err

    let reApplyX(re: string, s: string) = 
        // strips whitespace before applying a reg exp       
        match parseWhitespace(s) with 
        | ParseOK (_, remaining) -> 
            match reApply(re, remaining) with 
            | Ok maybeMatch, remaining' -> Ok maybeMatch, remaining'
            | (Error msg), remaining'-> Error msg, remaining'
        | _ -> 
            printfn "We should not be coming here a fail on whitespace should not happen"
            Ok None, s

    let parseNumber(s: string) =
        printfn "ParseNumber %s" s
        let reNumber: string = @"^([0-9\.]+)"
        let newValueResult, remaining = reApplyX(reNumber, s)  
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
                printfn "parse number failed on %s" s
                ParseOK (None, s)
        
        | Error err ->
            ParseError err


    let parseTag(s:string) =
        let reTag: string = @"^\'(.+?)\'"    //tags are strings enclosed in single quotes.  same as for attrib path for pipe character
        let newValueResult, remaining = reApplyX(reTag, s)      
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
        let reString: string = sprintf @"^\%c(.+)\%c" quot quot // strings are enclosed in double quotes
        let newValueResult, remaining = reApplyX(reString, s)      
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
        
    let parseIfThenElse() = 
        // to be done. this re gets between if and end on a multiline input
        // after which we would want to separate out on then and else
        //the first clause would split on an equality operator
        // perhapsif  the else  clause is  not given then default to noOutput.

        let re = @"^\s*if \s*([^-~]+)\s*end"
        re


    let parseFunctionName(s:string) =
        let reString: string = sprintf @"^([A-Za-z][A-Za-z0-9]*)\s*\(" //function name starts with alpha optionally continues with alphaNum and terminates with open parenthesis
        let newValueResult, remaining = reApplyX(reString, s)
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
                    printfn "Do we get here: %s, %s" str remaining
                    let remaining' = "(" + remaining //reApply swallows the opening bracket so replace it here
                    ParseOK (maybeFuncName, remaining')
                    
        | Error msg -> ParseError msg
        
        
    let parseBrackets(s:string) =
        // we could replace all the reg exps with character parsing except that would probably look  more like the voice analyser
        let rec processString(chars: list<char>, bracketCount: int, acc: list<char>) =
            match chars with 
            | [] -> 
                match bracketCount > 0 with 
                | true -> 
                    let msg = "Ran out of letters in parse Brackets - no closing bracket"
                    printfn "%s" msg
                    ParseError msg
                | false -> 
                    printfn  "Empty string?"
                    ParseOK (None, s) // an empty string must have been passed in to parseBrackets which would  be odd
            | '(' :: t -> 
                match bracketCount with 
                | 0 -> processString(t, bracketCount + 1, acc) // don't capture the first opening bracket
                | _ -> processString(t, bracketCount + 1, '(' :: acc) // capture internal brackets
            | ')' ::  t -> 
                match bracketCount with 
                | 0 -> 
                    let msg = "close bracket before open bracket"
                    ParseError msg
                | 1 -> 
                    let str = System.String.Concat(Array.ofList(List.rev acc))
                    let remaining = System.String.Concat(Array.ofList(t))
                    ParseOK (Some str, remaining) 
                | _ -> processString(t, bracketCount - 1, ')' :: acc)
            | c :: t -> 
                match bracketCount = 0 with
                | true -> 
                    printfn "First character is not a bracket: %c" c
                    ParseOK (None, s)
                | false -> processString(t, bracketCount, c :: acc)
            

        match parseWhitespace(s) with 
        | ParseOK (_, remaining) ->
            printfn "Processing %s" remaining            
            let letters = remaining |> Seq.toList
            processString(letters, 0, [])
        | ParseError msg -> ParseError msg


    

    let parseOperator(s:string) =
        let reOp = @"^([+-\/\*\^%])"
        let newValueResult, remaining = reApplyX(reOp, s)      
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
        // let rootOp = makeRootOp()
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
                                
                                let rhsPrec = // things more complex with comparative operators which don't have precedence
                                    match rhsOp.Operator with 
                                    | Operator (_rhsSym, rhsPrec) -> rhsPrec
                                    | Comparator _j -> 0 // comarator as rhs?
                                let lhsPrec = 
                                    match lhsOp.Operator with 
                                    | Operator (_lhsSym, lhsPrec) -> lhsPrec
                                    | Comparator _j -> 0 // comarator as lhs?
                                
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
                        | Expecting.BinOp ->
                            match term with 
                            | Value _v -> 
                                let msg = "Expecting Binary Operator, but got value"
                                Error msg
                            | BinaryOp binOp ->
                                // we have got what we were expecting - add this operator to binOps list
                                // printfn "Adding bin op to list %A" binOp
                                let binOps' = binOp :: binOps
                                gatherTerms(remaining', values, binOps', Expecting.Val (Unary opPlus))
                        | Expecting.Val unary ->
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
                                | Unary (Operator (ArithmeticSymbol (Plus, _), _)) ->
                                    // add the value as is to values list
                                    let values' = (v, dt) :: values
                                    gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                | Unary (Operator (ArithmeticSymbol (Minus, _), _)) ->
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
                        let msg = sprintf "Unable to parse expression %s after %s, remaining: [%s]" expr input remaining
                        Error msg
                | Error msg -> 
                    Error msg

        let result = gatherTerms(expr, [], [], Expecting.Val (Unary opPlus))
        printfn "%A" result


        match result with 
        | Ok (typedValues, binaryOps) ->
            // the initial term should be a value
            // now process the lists into a calc tree
            let astRes = buildAST(binaryOps, typedValues)
            // printfn "%A" astRes
            astRes

        | Error msg ->
            printfn "%s" msg
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
        | ParseOK (maybeFuncName, remaining) -> 
            match maybeFuncName with
            | None -> 
                Ok (None, input)
            | Some funcName ->
                let parseRes = parseBrackets(remaining)
                match parseRes with 
                | ParseOK (maybeMatch', remaining') -> 
                    match maybeMatch' with 
                    | Some parameters ->
                        match parameters.Trim().Length > 0 with 
                        | false->
                            let f = (Value (Function (funcName, [])), DataType.Numeric)
                            Ok (Some f, remaining')
                        |true -> 
                            let expressions = parameters.Split ','
                            // create a root binary operator for each parameter
                            printfn "Not expecting to get here"
                            let results = 
                                expressions |> 
                                List.ofArray |>
                                List.map parseExpression |>
                                List.rev                            
                                
                            // now run through the results of each paramter from list<results>
                            // | Function of string * list<TypedTerm> // labelled bracketed expression
                            
                            let fTermsRes =
                                results |>
                                List.fold(fun (acc: Result<list<Term * DataType>, string>) (res:Result<(Term * DataType),string>) ->
                                    match acc with 
                                    | Ok tts ->
                                        match res with 
                                        | Ok tt ->
                                            Ok (tt :: tts)
                                        | Error msg -> Error msg
                                    | Error msg -> Error msg
                                ) (Ok [])

                                // Ok (Some (term, DataType.Numeric), remaining) // assume that attributes return numeric values for the moment
                            match fTermsRes with 
                            | Ok tts -> 
                                // not sure how we will know the return type  of a function unless it is registered in some way
                                let f = (Value (Function (funcName, tts)), DataType.Numeric)
                                Ok (Some f, remaining')
                            | Error msg -> Error msg

                    | None -> 
                        Ok (None, input)

                | ParseError msg -> Error msg

        | ParseError msg ->
            Error msg

    let getQueue(t: Term) = 
        match t with 
        | Value v-> 
            match v with 
            | BinaryOpValue _bop -> DataQueue.Output
            | _-> DataQueue.Input
        | BinaryOp _bop -> DataQueue.Output


    let rec processCalcTree<'T>((term, dt): TypedTerm, inputs: list<Value * DataType>, operators:list<CalcOp<'T>>, opMap: Map<BinaryOperator, OpFunc<'T>> ) : list<Value * DataType> * list<CalcOp<'T>> = 
        // code in here is ugly due to a binary tree being both a binaryOp and a Value
        // simplifying the code, though  means duplicating all  of the data structures
        // which is also inelegant, but probably the lesser of two evils

        match term with 
        | Value v ->
            match v with 
            | BinaryOpValue bop -> 
                // recast as BinaryOp and call processTree again.
                processCalcTree((BinaryOp bop, dt), inputs, operators, opMap)
            | _ -> 
                (v, dt) :: inputs, operators

        | BinaryOp bop ->             
            match bop.LHS, bop.RHS with 
            | Some (lhsTerm, lhsDT), Some (rhsTerm, rhsDT) -> 
                let lhsQueue = getQueue(lhsTerm)                   
                let rhsQueue = getQueue(rhsTerm)
                let sym = bop.Operator
                let funcImpl =  Map.find sym opMap // we could check here that lhsDT and rhsDT are the same
                let operators' = (funcImpl, lhsQueue, rhsQueue) :: operators

                // if lhs is a value, need to lay that down before processing rhs, unless rhs is a value in which case it goes down first
                match rhsTerm with 
                | BinaryOp _rBop -> 
                    printfn "rhs term is binary"

                    match lhsTerm with
                    | BinaryOp _lBop -> 
                        // these are both binary ops - process the rhs first
                        printfn "lhs term is binary"
                        let (inputs', operators'') = processCalcTree((rhsTerm, rhsDT), inputs, operators', opMap)
                        processCalcTree((lhsTerm, lhsDT), inputs', operators'', opMap)
                    | Value v ->
                        printfn "lhs term is value"

                        match v with 
                        | BinaryOpValue _bopV -> 
                            printfn "lhs term is binary VALUE"
                            // this is actually a binary operator so process rhs first
                            let (inputs', operators'') = processCalcTree((rhsTerm, rhsDT), inputs, operators', opMap)
                            processCalcTree((lhsTerm, lhsDT), inputs', operators'', opMap)
                        | _ ->  
                            // lay lhs value down in inputs before processing rhs
                            printfn "Are we getting  here??"
                            let (inputs', operators'') = processCalcTree((lhsTerm, lhsDT), inputs, operators', opMap)
                            processCalcTree((rhsTerm, rhsDT), inputs', operators'', opMap)
                | Value v ->
                    printfn "rhs term is value"
                    match v with 
                    | BinaryOpValue _bopV -> 
                        printfn "rhs term is binary VALUE"
                        //check if lhs is value
                        match lhsTerm with 
                        | BinaryOp _lBop -> 
                            // these are both binary ops - process the rhs first
                            printfn "lhs term is binary"
                            let (inputs', operators'') = processCalcTree((rhsTerm, rhsDT), inputs, operators', opMap)
                            processCalcTree((lhsTerm, lhsDT), inputs', operators'', opMap)
                        | Value v ->
                            printfn "lhs term is value"

                            match v with 
                            | BinaryOpValue _bopV -> 
                                printfn "lhs term is binary VALUE"
                                // this is actually a binary operator so process rhs first
                                let (inputs', operators'') = processCalcTree((rhsTerm, rhsDT), inputs, operators', opMap)
                                processCalcTree((lhsTerm, lhsDT), inputs', operators'', opMap)
                            | _ ->  
                                // lay lhs value down in inputs before processing rhs
                                printfn "Are we getting  here??"
                                let (inputs', operators'') = processCalcTree((lhsTerm, lhsDT), inputs, operators', opMap)
                                processCalcTree((rhsTerm, rhsDT), inputs', operators'', opMap)
                    | _ ->
                        // process the rhs first regardless of lhs
                        let (inputs', operators'') = processCalcTree((rhsTerm, rhsDT), inputs, operators', opMap)
                        processCalcTree((lhsTerm, lhsDT), inputs', operators'', opMap)
                    

            | _ -> 
                //we will have to wrap this up in a Result, but for now log and drop out
                // alternatively we could prevalidate and work on validated structures
                printfn "Error:  Need LHS and RHS in Binary Operator %A" bop
                inputs, operators


    let plus (a: float, b:float) =
        printfn "adding %f, %f" a b
        a + b

    let minus (a: float, b:float) =
        printfn "subtracting %f, %f" a b

        a - b

    let multiply (a: float, b:float) =
        printfn "multiplying %f, %f" a b

        a * b

    let divide (a: float, b:float) =
        printfn "dividing %f, %f" a b

        a / b

    let power (a: float, b:float) =
        a ** b
    
    let modulo (a: float, b:float) =
        a % b

    let getValue<'T>(dataQ, inputs: list<'T>, outputs: list<'T>) = 
        match dataQ with 
        | DataQueue.Input ->
            match inputs with 
            | [] -> 
                printfn "No value in inputs"
                None, inputs, outputs
            | h :: t -> (Some h, t, outputs)
        | DataQueue.Output ->
            match outputs with 
            | [] -> 
                printfn "No value in outputs"
                None, inputs, outputs
            | h :: t -> Some h, inputs, t


    let createCalcEvaluator<'T>(ops:list<CalcOp<'T>>) = 
        //pass in number of args? tk
        // how will this work with functions, where the inputs may have different types? tk
        // ideally functions will not have to unbox all their inputs, but that might be the only way to do it
        fun(inputs: list<'T>) ->
            // validate number of inputs?
            // iterate through each calOp and pass it the inputs and outputs lists
            // let inputsRev = List.rev inputs
            let outputs: list<'T> = []
            let (ins, outs) =
                ops |>
                List.fold(fun (inAcc, outAcc) (opFunc, lhsQ, rhsQ) -> 
                    let (lhsVal: option<'T>, inputs': list<'T>,  outputs': list<'T>) = getValue(lhsQ, inAcc, outAcc)
                    let (rhsVal: option<'T>, inputs'': list<'T>,  outputs'': list<'T>) = getValue(rhsQ, inputs', outputs')
                    match (lhsVal, rhsVal) with 
                    | (Some lhs, Some rhs) ->
                        let t:'T = opFunc(lhs, rhs)
                        (inputs'', t :: outputs'')
                    | _ -> 
                        printfn "Not enough values" // we should validate initial input length at the beginning
                        (inputs, outputs)
                ) (inputs, outputs)

            // check if there are unused inputs
            
            match ins with 
            | _h :: _ -> Error "Unused inputs"
            | _ -> 
                match outs with 
                | [] ->  Error "No outputs"
                | [h] ->
                    Ok h
                | _ ->
                    Error "Unused inputs"

            

        


    let testParseExpression(expr:string) = 
        let exprRes = parseExpression(expr) // these values are just placeholders though they are returned from processCalcTree

        match exprRes with 
        | Ok binOp -> 
            printfn "%A" binOp
            let opMap = 
                [
                    (opPlus, plus)
                    (opMinus, minus)
                    (opMultiply, multiply)
                    (opDivide, divide)
                    // (Operator NoOp, this would be an error)
                    (opPower, power)
                    (opModulo, modulo)
                ] 
                |> Map.ofList
                            
            let inputs, operators = processCalcTree(binOp, [], [], opMap)
            
            printfn "%A" inputs
            printfn "%A" operators
            
            let evaluator = createCalcEvaluator(operators)
            
            fun (ts: list<float>) ->
                match ts |> evaluator  with 
                | Ok v -> v
                | Error msg ->  
                    printfn "%s" msg
                    infinity
            // Ok (inputs, operators)
        | Error msg -> 
            fun (ts: list<float>) ->
                printfn "Error: %s" msg
                infinity

    let rec expressionFromTerm(term: Term) = 
        // we need to know if operators are associative or not (a - b) - c != a - (b - c)
        let valueToString(v: Value) = 
            match v with 
            | Tag tag -> sprintf @"'%s'" tag
            | Constant c -> 
                match c with 
                | StringConst strConst -> strConst
                | NumericalConst numConst -> (string) numConst

            | Path path -> path
            | BinaryOpValue binOp -> 
                //  would not expect to come here when serialising an expression tree but add brackets for  now which is how binary op values are created
                printfn "We have a BinaryOpValue in expressionFromTerm - which is unexpected"
                expressionFromTerm(BinaryOp binOp)
                |> fun s -> "(" + s + ")" 

            | Function (fName, _) -> sprintf "Function: %s" fName
        
        // let b: BinaryOp = 9
        let rec dodah(acc: list<string>, t: Term, parentPrecedence: Precedence) : list<string>= 
            match t with 
            | Value v -> 
                let  s = valueToString(v)
                s :: acc
                
            | BinaryOp bOp ->
                let thisPrec, thisAssoc = 
                    match bOp.Operator with 
                    | Operator (ArithmeticSymbol(_sym, isAssoc) , prec) -> prec, isAssoc
                    | Comparator _comparator ->(0, false) //grouping always matters with a comparator
                
                let acc2 = 
                    match parentPrecedence > thisPrec with 
                    | true -> "{" :: acc // we may never get here as any input in brackets becomes an opValue
                    | false -> acc 
                
                let accLHS = 
                    match bOp.LHS with 
                    | Some (lhsTerm,  _lhsDT) -> 
                        match lhsTerm with 
                        | Value v ->  
                            let  s = valueToString(v)
                            s :: acc2

                        | BinaryOp lhsOp -> 
                            dodah(acc2, BinaryOp lhsOp, thisPrec)

                    | None -> "Error no LHS term" :: acc2 // this would be an error - we should return a result

                let accOp = bOp.Operator.OpToString() :: accLHS

                let accRHS= 
                    match bOp.RHS with 
                    | Some (rhsTerm,  _rhsDT) -> 
                
                        match rhsTerm with 
                        | Value v ->  
                            let  s = valueToString(v)
                            s :: accOp

                        | BinaryOp rhsOp -> 
                                    
                            let precedence = 
                                match thisAssoc with 
                                | true -> thisPrec
                                | false -> 99 //force rhs to wrap itself in parentheses
                            dodah(accOp, BinaryOp rhsOp, thisPrec)
                            
                    | None -> "Error no RHS term" :: acc // this would be an error - we should return a result
                        
            
                match parentPrecedence > thisPrec with 
                | true -> "}" :: accRHS
                | false -> accRHS

                
        dodah([], term, -1)
        |> List.rev
        |> List.fold (+) ""
                
