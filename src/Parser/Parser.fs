namespace Parser

module CalcParser =
    open ParserTypes
    open System.Text.RegularExpressions
    open Tweega.Utils    
    [<RequireQualifiedAccessAttribute>]
    type TermType =
    | Float of IntegralPart * FractionPart
    | String of string

    let quot = '\u0022'    

    let determinePrecision (a: NumericValue) (b: NumericValue) : Number =
        // Determines the highest precision between two values
        match a, b with
        | NumericValue.Float64 _, _ | _, NumericValue.Float64 _ -> Number.Float64
        | NumericValue.Float32 _, _ | _, NumericValue.Float32 _ -> Number.Float32
        | NumericValue.Int64 _, _ | _, NumericValue.Int64 _ -> Number.Int64
        | NumericValue.Int32 _, _ | _, NumericValue.Int32 _ -> Number.Int32
        | NumericValue.Int16 _, _ | _, NumericValue.Int16 _ -> Number.Int16
        | NumericValue.Int8 _, _ | _, NumericValue.Int8 _ -> Number.Int8

    let castToOriginalPrecision (result: float) (precision: Number) : NumericValue =
        match precision with
        | Number.Float64 -> NumericValue.Float64 result
        | Number.Float32 -> NumericValue.Float32 (float32 result)
        | Number.Int64 -> NumericValue.Int64 (int64 result)
        | Number.Int32 -> NumericValue.Int32 (int32 result)
        | Number.Int16 -> NumericValue.Int16 (int16 result)
        | Number.Int8 -> NumericValue.Int8 (int8 result)

    let toFloat (value: NumericValue) : float =
        match value with
        | NumericValue.Float64 f -> f
        | NumericValue.Float32 f -> float f
        | NumericValue.Int64 i -> float i
        | NumericValue.Int32 i -> float i
        | NumericValue.Int16 i -> float i
        | NumericValue.Int8 i -> float i


    let liftDataType(dt: DataType) = 
        // This function takes a data type and where that data  type is numeric, wraps it in a function that casts it to a float


        ()


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

    let stripLeadingWhitespace(str:string) =
        match parseWhitespace(str) with 
        | ParseOK (_, remaining) -> 
            remaining
        | ParseError err ->
            printfn "We should not be failing on stripping whitespace: %s" err
            str

    let reApplyX(re: string, s: string) = 
        // strips whitespace before applying a reg exp 
        let s' = stripLeadingWhitespace(s)      
        match reApply(re, s') with 
        | Ok maybeMatch, remaining -> Ok maybeMatch, remaining
        | (Error msg), remaining-> Error msg, remaining

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


    let parseField(s:string) =
        let reField: string = @"^\'(.+?)\'"    //fields are strings enclosed in single quotes.  same as for attrib path for pipe character
        let newValueResult, remaining = reApplyX(reField, s)      
        match newValueResult with 
        | Ok maybeField -> 
            match maybeField with 
            | None -> 
                ParseOK (None, s)
            | Some field ->
                match field with 
                | "" ->
                    let msg = "Error: Empty field name"
                    ParseError msg
                | _ ->
                    let illegalChars = sprintf "%s%c" @"*'\?;{}[\]\|\\`\" quot //we wouldn't actually detect single quote here
                    let reIllegal = sprintf "[%s]" illegalChars
                    match reApply(reIllegal, field) with 
                    | Ok (Some x),_s -> 
                        let msg = sprintf "Illegal character (%s)in field name: %s" x s
                        ParseError msg
                    | _ ->
                        let reNonPrintable = @"[^ -~]"
                        match reApply(reNonPrintable, field) with 
                        | Ok (Some _), _s -> 
                            let msg = sprintf "Non printable character in field name: %s" s
                            ParseError msg
                        | _ ->
                            ParseOK (maybeField, remaining)
                    
        | Error msg -> ParseError msg
        

    let parseString(s:string) =
        let reString: string = sprintf @"^\%c(.+)\%c" quot quot // strings are enclosed in double quotes
        let newValueResult, remaining = reApplyX(reString, s)      
        match newValueResult with 
        | Ok maybeStr -> 
        
            match maybeStr with 
            | None -> ParseOK (None, s)

            | Some str ->
                let reNonPrintable = @"[^ -~]"
                match reApply(reNonPrintable, str) with 
                | Ok (Some _), _s -> 
                    let msg = sprintf "Non printable character in string: %s" s
                    ParseError msg
                | _ ->
                    ParseOK (maybeStr, remaining)
                    
        | Error msg -> ParseError msg
        
    
    let parseFunctionName(s:string) =
        let reString: string = sprintf @"^([A-Za-z][A-Za-z0-9]*)\s*\(" //function name starts with alpha optionally continues with alphaNum and terminates with open parenthesis
        let newValueResult, remaining = reApplyX(reString, s)
        match newValueResult with 
        | Ok maybeFuncName -> 
        
            match maybeFuncName with 
            | None ->ParseOK (None, s)

            | Some str ->
                let reNonPrintable = @"[^ -~]"
                match reApply(reNonPrintable, str) with 
                | Ok (Some _), _s -> 
                    let msg = sprintf "Non printable character in function  name: %s" s
                    ParseError msg
                | _ ->
                    printfn "Do we get here: %s, %s" str remaining
                    let remaining' = "(" + remaining //reApply swallows the opening bracket so replace it here
                    ParseOK (maybeFuncName, remaining')
                    
        | Error msg -> ParseError msg
        
        
    let parseBrackets(s:string) : ParseResult =
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

    let parseConditional(s:string) =
        let reString: string = @"^if\s+(.+?)\s+then\s+" 
        let newValueResult, remaining = reApplyX(reString, s)      
        match newValueResult with 
        | Ok maybeStr -> 
        
            match maybeStr with 
            | None -> ParseOK (None, s)

            | Some predicate ->
                // get a reverse of the rest of the string and look for "esle" - this is only valid if the else clause is the rest of the expression
                // see if s contains an "End If" - in which case we will need to parse this differently
                // for the moment assume that we don't have End Ifs in which case this if clause must be the rest of the expression
                let hasEndIf = false
                match hasEndIf with 
                | true -> 
                    ParseError "Have not implemented End Ifs yet"
                | false -> 
                let revS = reverseString(remaining)
                let reElse = @"^(.+?)\s+esle\s+"
                match reApplyX(reElse, revS) with 
                | Ok (Some elseClauseRev), onSuccessRev -> 
                    let onFail = reverseString(elseClauseRev)
                    let onSuccess = reverseString(onSuccessRev)
                    let resultStr = sprintf "%s:%s:%s" predicate onSuccess onFail

                    ParseOK (Some resultStr, "") // unless we have end if statements, there won't be any remaining - we should have used up the rest of the expression
                | _ ->
                    let msg = sprintf "No else clause in string: %s for predicate %s" s predicate
                    ParseError msg
                    
        | Error msg -> ParseError msg

    

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
                let term = str |> (StringConst >> Value.Constant >> Term.Value)
                
                Ok (Some (term, DataType.String), remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    let parseAndHandleNumber(input: string) =
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> (NumericalConst >> Value.Constant >> Term.Value)
                // assume that all numbers are float32 for the moment 0 this needs to change tk
                Ok (Some (term, DataType.Numeric Number.Float64), remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    let parseAndHandleField(input: string) =
        match parseField(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Value.Field >> Term.Value))
                // assume fields emit float values for the moment - this needs to change tk
                Ok (Some (term, DataType.Numeric Number.Float64), remaining) 
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    let parseAndHandlePath(input: string, terms: list<TypedTerm>) =
        match parseField(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Value.Path >> Term.Value))
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
                let binOp:BinaryOperator = 
                    match opStr with 
                    | "+" -> opPlus
                    | "-" -> opMinus
                    | "*" -> opMultiply
                    | "/" -> opDivide
                    | "%" -> opModulo
                    | "^" -> opPower
                    | _ -> noOp

                let binOp' = {
                    BinaryOp.Operator = binOp;
                    BinaryOp.LHS = None;
                    BinaryOp.RHS = None;
                } 
                
                let term = binOp' |> Term.BinaryOp

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

        let parseAndHandleValue = parseAndHandleField >=> parseAndHandleNumber >=> parseAndHandleString >=> parseAndHandleBrackets
        let parseAndHandleTerm = parseAndHandleValue >=> parseAndHandleBinaryOperator >=> parseAndHandleFunction >=> parseAndHandleConditional

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
                    Ok ((Term.Value  hVal), hDt)
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
                            | Term.Value _v ->
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
                                    let rhsTypedTerm =  (rhsOp' |> Term.BinaryOp, rhsDt)
                                    // let jj = { lhsOp with RHS =  Some rhsTypedTerm }
                                    // printfn "JJ:%A" jj
                                    { lhsOp with RHS =  Some rhsTypedTerm } |> Ok

                                | false ->
                                    printfn "false"
                                    // no precedence conflict so rhs becomes new head op with lhs set to current head 
                                    // taking data type from lhs - type validation should happen before this merge - an expression should only contain one type
                                    let lhsTypedTerm =  (lhsOp |> Term.BinaryOp, rhsDt)
                                    { rhsOp with LHS = Some lhsTypedTerm } |> Ok
                                
                            | Term.BinaryOp bop ->
                                printfn "We should not  be here yet"
                                let mergedOp = mergeOps(bop, rhsOp)
                                printfn "Merged BOP:%A" mergedOp
                                match mergedOp with 
                                | Ok bop' ->
                                    // put bop' into lhsOp.RHS
                                    let rhsTypedTerm =  ((bop' |> Term.BinaryOp), rhsDt)
                                    { lhsOp with RHS = Some rhsTypedTerm } |> Ok

                                | Error msg -> Error msg
                                
                    // put last value into lhs of head of accumulator, if there is one
                    let op = { hAcc with LHS = Some ((Term.Value hVal), hDt) }
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
                        Ok ((bop' |> Term.BinaryOp), hDt)
                    | Error msg -> 
                        // printfn "Failure: %s" msg
                        Error msg
            
            | hOp :: tOp, (hVal, hDt) :: tVal -> 
                //  the accumulator is a list of operators with rhs set to value

                let op = { hOp with RHS = Some ((Term.Value hVal), hDt) }

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
                            | Term.Value _v -> 
                                let msg = "Expecting Binary Operator, but got value"
                                Error msg
                            | Term.BinaryOp binOp ->
                                // we have got what we were expecting - add this operator to binOps list
                                // printfn "Adding bin op to list %A" binOp
                                let binOps' = binOp :: binOps
                                gatherTerms(remaining', values, binOps', Expecting.Val (Unary opPlus))
                        | Expecting.Val unary ->
                            match term with 
                            | Term.BinaryOp bop ->
                                // treat this as a unary operator
                                // printfn "Treating as unary!"
                                match Unary.Combine(unary, Unary bop.Operator) with 
                                | Ok unary' -> 
                                    gatherTerms(remaining', values, binOps, Expecting.Val unary')    
                                | Error msg -> Error msg
                            | Term.Value v ->
                                match unary with 
                                | Unary (Operator (Plus, _)) ->
                                    // add the value as is to values list
                                    let values' = (v, dt) :: values
                                    gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                | Unary (Operator (Minus, _)) ->
                                    // multiply value by -1 if it is a constant number 
                                    // check  that dt is Numeric otherwose error
                                    match v with 
                                    | Value.Constant (NumericalConst nStr) ->     
                                        let maybeN =                                    
                                            match System.Double.TryParse(nStr) with 
                                            | true, n ->  Some ((string) (n * -1.0))
                                            | _ -> 
                                                // we should probably throw an error here tk
                                                None
                                        match maybeN with 
                                        | Some n' ->
                                            let v' = n' |> (NumericalConst >> Value.Constant)

                                            let values' = (v', dt) :: values
                                            gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                        | None -> 
                                            Error "Unable to parse float constant"
                                    | _->
                                        // we need to handle other numeric types such as field here tk
                                        let minusOneOp = (string) -1 |> (NumericalConst >> Value.Constant)
                                        let lhs' = (Term.Value minusOneOp, Number.Float64 |> DataType.Numeric) |> Some

                                        let op = {
                                            BinaryOp.Operator = opMultiply;
                                            BinaryOp.LHS = lhs';
                                            BinaryOp.RHS = Some (term, Number.Float64 |> DataType.Numeric);
                                        }
                                        let bopVal = Value.BinaryOpValue op
                                        let values' = (bopVal, dt) :: values
                                        gatherTerms(remaining', values', binOps, Expecting.BinOp)
                                | _ -> Error "Only Plus and Minus can be used as unary operators"


                    | None -> 
                        let msg = sprintf "Unable to parse expression %s after %s, remaining: [%s]" expr input remaining
                        Error msg
                | Error msg -> 
                    Error msg

        let result = gatherTerms(expr, [], [], Expecting.Val (Unary opPlus))
        // printfn "%A" result


        match result with 
        | Ok (typedValues, binaryOps) ->
            // the initial term should be a value
            // now process the lists into a calc tree
            let astRes = buildAST(binaryOps, typedValues)
            printfn "%A" astRes
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
                        | Term.Value _v -> term
                        | Term.BinaryOp bop -> 
                            Term.Value (Value.BinaryOpValue bop)
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
                let parseRes:ParseResult = parseBrackets(remaining)
                match parseRes with 
                | ParseOK (maybeMatch', remaining') -> 
                    match maybeMatch' with 
                    | Some parameterString ->
                        match parameterString.Trim().Length > 0 with 
                        | false->
                            let f = (Term.Value (Value.Function (funcName, [])), DataType.Numeric Number.Float64)
                            Ok (Some f, remaining')
                        |true -> 
                            let parameters = parameterString.Split ','
                            // create a root binary operator for each parameter
                            printfn "Not expecting to get here"
                            // parse each parameter expression
                            let results = 
                                parameters |> 
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
                                let returnType = DataType.Numeric Number.Float64
                                let typedTerm = (Term.Value (Value.Function (funcName, tts)), returnType)
                                Ok (Some typedTerm, remaining')
                            | Error msg -> Error msg

                    | None -> 
                        Ok (None, input)

                | ParseError msg -> Error msg

        | ParseError msg ->
            Error msg

    and parseAndHandleConditional(input: string) =
        match parseConditional(input) with 
        | ParseOK (cond, remaining) -> 
            match cond with
            | None -> 
                Ok (None, input)
            | Some conditional ->
                let parts = conditional.Split [|':'|] |> List.ofArray
                match parts with 
                | predicate :: success :: fail :: [] ->
                    let predicateResult = parseExpression(predicate)
                    let successResult = parseExpression(success)
                    let failResult = parseExpression(fail)
                    predicateResult |>
                    Result.bind(fun predicateTerm -> 
                        successResult |>
                        Result.bind(fun successTerm -> 
                            failResult |>
                            Result.bind(fun failTerm ->
                                printfn "We have enough to make a term from a conditional"
                                let cond = {
                                    Conditional.Predicate = predicateTerm;
                                    Conditional.OnSuccess = successTerm;
                                    Conditional.OnFail = failTerm;
                                }
                                let term = cond |> (Value.Conditional >> Term.Value)
                                // how do I know the return type of this conditional
                                // the success and fail branches should agree, though in AF they don't have to
                                // we also have functions such as NoOutput and Exit() - which are side effects
                                Ok (Some (term, DataType.Numeric Number.Float64), remaining)
                            )
                        )
                    
                    )
                | _ -> 
                    let err = "Wrong number of parts returned from parseConditional"
                    printfn "%s" err
                    Error err


        | ParseError msg ->
            Error msg

    let getQueueType(t: Term) = 
        match t with 
        | Term.Value v-> 
            match v with 
            | Value.BinaryOpValue _bop -> QueueType.Output
            | _-> QueueType.Input
        | Term.BinaryOp _bop -> QueueType.Output


    let rec processCalcTree<'T>(
        (term, dt): TypedTerm, 
        inputs: list<Value * DataType>, 
        calcOps:list<BinaryCalcOp>, 
        opMap: Map<BinaryOperator, Mappend<ResolvedValue>>) : list<Value * DataType> * list<BinaryCalcOp> = 

        // code in here is ugly due to a binary tree being both a binaryOp and a Value
        // simplifying the code, though  means duplicating all  of the data structures
        // which is also inelegant, but probably the lesser of two evils

        // already when considering including comparators alongside arithmetic functions, we have a clash of types
        // implementations in this map need to be from obj -> obj -> obj

        // analyses typed term 

        match term with 
        | Term.Value v ->
            match v with 
            | Value.BinaryOpValue bop -> 
                // recast as BinaryOp and call processTree again.
                processCalcTree((Term.BinaryOp bop, dt), inputs, calcOps, opMap)
            | _ -> 
                (v, dt) :: inputs, calcOps

        | Term.BinaryOp bop ->             
            match bop.LHS, bop.RHS with 
            | Some (lhsTerm, lhsDT), Some (rhsTerm, rhsDT) -> 
                let lhsQueueType = getQueueType(lhsTerm)    // either an input from 'user' or an output from a sub calculation               
                let rhsQueueType = getQueueType(rhsTerm)
                let sym = bop.Operator
                let funcImpl =  Map.find sym opMap // we could check here that lhsDT and rhsDT are of the same type
                let calcOps' = (funcImpl, lhsQueueType, rhsQueueType) :: calcOps

                // if lhs is a value, need to lay that down before processing rhs, unless rhs is a value in which case it goes down first
                match rhsTerm with 
                | Term.BinaryOp _rBop -> 
                    printfn "rhs term is binary"

                    match lhsTerm with
                    | Term.BinaryOp _lBop -> 
                        // these are both binary ops - process the rhs first
                        printfn "lhs term is binary"
                        let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps', opMap)
                        processCalcTree((lhsTerm, lhsDT), inputs', calcOps'', opMap)
                    | Term.Value v ->
                        printfn "lhs term is value"

                        match v with 
                        | Value.BinaryOpValue _bopV -> 
                            printfn "lhs term is binary VALUE"
                            // this is actually a binary operator so process rhs first
                            let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps', opMap)
                            processCalcTree((lhsTerm, lhsDT), inputs', calcOps'', opMap)
                        | _ ->  
                            // lay lhs value down in inputs before processing rhs
                            printfn "Are we getting  here??"
                            let (inputs', calcOps'') = processCalcTree((lhsTerm, lhsDT), inputs, calcOps', opMap)
                            processCalcTree((rhsTerm, rhsDT), inputs', calcOps'', opMap)
                | Term.Value v ->
                    printfn "rhs term is value"
                    match v with 
                    | Value.BinaryOpValue _bopV -> 
                        printfn "rhs term is binary VALUE"
                        //check if lhs is value
                        match lhsTerm with 
                        | Term.BinaryOp _lBop -> 
                            // these are both binary ops - process the rhs first
                            printfn "lhs term is binary"
                            let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps', opMap)
                            processCalcTree((lhsTerm, lhsDT), inputs', calcOps'', opMap)
                        | Term.Value v ->
                            printfn "lhs term is value"

                            match v with 
                            | Value.BinaryOpValue _bopV -> 
                                printfn "lhs term is binary VALUE"
                                // this is actually a binary operator so process rhs first
                                let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps', opMap)
                                processCalcTree((lhsTerm, lhsDT), inputs', calcOps'', opMap)
                            | _ ->  
                                // lay lhs value down in inputs before processing rhs
                                printfn "Are we getting  here??"
                                let (inputs', operators'') = processCalcTree((lhsTerm, lhsDT), inputs, calcOps', opMap)
                                processCalcTree((rhsTerm, rhsDT), inputs', operators'', opMap)
                    | _ ->
                        // process the rhs first regardless of lhs
                        let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps', opMap)
                        processCalcTree((lhsTerm, lhsDT), inputs', calcOps'', opMap)
                    

            | _ -> 
                //we will have to wrap this up in a Result, but for now log and drop out
                // alternatively we could prevalidate and work on validated structures
                printfn "Error:  Need LHS and RHS in Binary Operator %A" bop
                inputs, calcOps

    let plus (a: float, b:float) =
        // printfn "adding %f, %f" a b
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

    
    let makeComparatorOfT (comparator:ComparatorSymbol) (a: 'T, b:'T) =
        match comparator with 
        | ComparatorSymbol.Equals -> 
            a = b
        | ComparatorSymbol.LessThan -> 
            a < b
        | ComparatorSymbol.LessThanOrEquals -> 
            a <= b
        | ComparatorSymbol.GreaterThan -> 
            a > b
        | ComparatorSymbol.GreaterThanOrEquals -> 
            a >= b

    let getValue<'T>(dataQ:QueueType, inputs: list<'T>, outputs: list<'T>) = 
        match dataQ with 
        | QueueType.Input ->
            match inputs with 
            | [] -> 
                printfn "No value in inputs"
                None, inputs, outputs
            | h :: t -> (Some h, t, outputs)
        | QueueType.Output ->
            match outputs with 
            | [] -> 
                printfn "No value in outputs"
                None, inputs, outputs
            | h :: t -> Some h, inputs, t

    // input is a list of calc ops (mappend that merges 2 values of the same type, 
    // plus 2 queues - one that holds values fed into the top level function, 
    // and another that holds values for sub functions
    // returns a function that will take a list of arguments (all of type 'T)
    // when called, the function folds over the calc ops
    // getValue looks at next item in the queue and if this is of type input gets a value from the input queue, otherwise from the output queue (which is results of calcs)


    let createCalcEvaluator<'T>(ops:list<BinaryCalcOp>) = 
        //pass in number of args? tk
        // how will this work with functions, where the inputs may have different types? tk
        // ideally functions will not have to unbox all their inputs, but that might be the only way to do it
        fun(inputs: list<ResolvedValue>) ->
            // validate number of inputs?
            // iterate through each calcOp and pass it the inputs and outputs lists
            // let inputsRev = List.rev inputs
            let outputs: list<ResolvedValue> = []
            let (ins, outs) =
                ops |>
                List.fold(fun (inAcc, outAcc) (mappend, lhsQ, rhsQ) -> 
                    let (lhsVal: option<ResolvedValue>, inputsLHS: list<ResolvedValue>,  outputsLHS: list<ResolvedValue>) = 
                        getValue(lhsQ, inAcc, outAcc)
                    let (rhsVal: option<ResolvedValue>, inputsRHS: list<ResolvedValue>,  outputsRHS: list<ResolvedValue>) = 
                        getValue(rhsQ, inputsLHS, outputsLHS)
                    match (lhsVal, rhsVal) with 
                    | (Some lhs, Some rhs) ->
                        let t:ResolvedValue = mappend(lhs, rhs)
                        (inputsRHS, t :: outputsRHS)
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
            printfn "binOp:\n%A" binOp
            // perhaps operators need not be part of this map - it is hard to imagine how their implementations might change
            let numericOps = 
                [
                    (opPlus, plus)  // these functions may need wrappers so they operate on a DU of float| string
                    (opMinus, minus)
                    (opMultiply, multiply)
                    (opDivide, divide)
                    // (Operator NoOp, this would be an error)
                    (opPower, power)
                    (opModulo, modulo)
                    // (opEq, makeComparator Equals)
                    // (opGT, makeComparator GreaterThan)
                    // (opGTE, makeComparator GreaterThanOrEquals)
                    // (opLT, makeComparator LessThan)
                    // (opLTE, makeComparator LessThanOrEquals)

                ]
                |> List.map(fun (binOp, floatImpl) -> 
                    // the intention here is to create a wrapping function that 'lifts' numeric values to floats
                    // then executes the float -> float -> float function, as in plus
                    // and then drops the value to a lower precision if appropriate
                    let numericHandler = fun (rv1:ResolvedValue, rv2:ResolvedValue) ->
                        match (rv1, rv2) with 
                        | ResolvedValue.Numeric a, ResolvedValue.Numeric b -> 
                            let float64A = (toFloat a) //|> LiftedValue.Numeric
                            let float64B = (toFloat b) //|> LiftedValue.Numeric
                            let float64Result = floatImpl(float64A, float64B)
                            // if neither original arguments was double reduce precision to highest precision of the inputs
                            let precision = determinePrecision a b
                            let droppedValue = castToOriginalPrecision float64Result precision
                            droppedValue |> ResolvedValue.Numeric
                        | ResolvedValue.String a, ResolvedValue.String b -> 
                            ResolvedValue.String (sprintf("%s%s") a b)
                        |  _ ->
                            let msg = sprintf "Invalid types for operator : %s.  Got (%s, %s)" (binOp.OpToString()) (rv1.ToString()) (rv2.ToString())
                            ResolvedValue.BadVal msg
                    
                    (binOp, numericHandler)
                ) 
                //|> Map.ofList

            let comparatorOps =                 
                [
                    (opEq, Equals)
                    (opGT,  GreaterThan)
                    (opGTE, GreaterThanOrEquals)
                    (opLT, LessThan)
                    (opLTE, LessThanOrEquals)

                ]
                |> List.map(fun (binOp, comparatorSymbol) -> 
                    // the intention here is to create a wrapping function that 'lifts' numeric values to floats
                    // then executes the float -> float -> float function, as in plus
                    // and then drops the value to a lower precision if appropriate
                    
                    let numericHandler = fun (rv1:ResolvedValue, rv2:ResolvedValue) ->
                        match (rv1, rv2) with 
                        | ResolvedValue.Numeric a, ResolvedValue.Numeric b -> 
                            let comparatorImpl = makeComparatorOfT comparatorSymbol

                            let float64A = (toFloat a) //|> LiftedValue.Numeric
                            let float64B = (toFloat b) //|> LiftedValue.Numeric
                            let boolResult = comparatorImpl(float64A, float64B)
                            boolResult |> ResolvedValue.Boolean
                        | ResolvedValue.String a, ResolvedValue.String b ->
                            let comparatorImpl = makeComparatorOfT comparatorSymbol
                            let boolResult = comparatorImpl(a, b)
                            boolResult |> ResolvedValue.Boolean
                        |  _ ->
                            let msg = sprintf "Invalid types for operator : %s.  Got (%s, %s)" (binOp.OpToString()) (rv1.ToString()) (rv2.ToString())
                            ResolvedValue.BadVal msg
                    
                    (binOp, numericHandler)
                ) 

            let joinedLists = List.Join([comparatorOps; numericOps])
            let opMap = joinedLists |> Map.ofList

            let (inputs: list<Value * DataType>), (operators: list<BinaryCalcOp>) = processCalcTree(binOp, [], [], opMap)
            
            printfn "%A" inputs
            printfn "%A" operators
            
            let evaluator = createCalcEvaluator(operators)
            
            fun (ts) ->
                match ts |> evaluator  with 
                | Ok v -> v
                | Error msg ->  
                    printfn "%s" msg
                    ResolvedValue.BadVal msg
            // Ok (inputs, operators)
        | Error msg -> 
            fun (ts) ->
                printfn "Error: %s" msg
                ResolvedValue.BadVal msg
                
    
    let rec expressionFromTerm(term: Term) = 
        let valueToString(v: Value) = 
            match v with 
            | Value.Field field -> sprintf @"'%s'" field
            | Value.Constant c -> 
                match c with 
                | StringConst strConst -> strConst
                | NumericalConst numConst -> (string) numConst

            | Value.Path path -> path
            | Value.BinaryOpValue binOp -> 
                //  would not expect to come here when serialising an expression tree but add brackets for  now which is how binary op values are created
                printfn "We have a BinaryOpValue in expressionFromTerm - which is unexpected"
                expressionFromTerm(Term.BinaryOp binOp)
                |> fun s -> "(" + s + ")" 

            | Value.Function (fName, _) -> sprintf "Function: %s" fName
            | Value.Conditional c -> sprintf "Conditional: %s" (c.Predicate.ToString())
        
        // let b: BinaryOp = 9
        let rec serialiseTerm(acc: list<string>, t: Term, parentPrecedence: Precedence) : list<string> = 
            match t with 
            | Term.Value v -> 
                let  s = valueToString(v)
                s :: acc
                
            | Term.BinaryOp bOp ->
                let thisPrec = 
                    match bOp.Operator with 
                    | Operator (_sym, prec) -> prec
                    | Comparator _comparator ->0 //grouping always matters with a comparator
                
                let acc2 = 
                    match parentPrecedence > thisPrec with 
                    | true -> "{" :: acc // we may never get here as any input in brackets becomes an opValue
                    | false -> acc 
                
                let accLHS = 
                    match bOp.LHS with 
                    | Some (lhsTerm,  _lhsDT) -> 
                        match lhsTerm with 
                        | Term.Value v ->  
                            let  s = valueToString(v)
                            s :: acc2

                        | Term.BinaryOp lhsOp -> 
                            serialiseTerm(acc2, Term.BinaryOp lhsOp, thisPrec)

                    | None -> "Error no LHS term" :: acc2 // this would be an error - we should return a result

                let accOp = bOp.Operator.OpToString() :: accLHS

                let accRHS= 
                    match bOp.RHS with 
                    | Some (rhsTerm,  _rhsDT) -> 
                
                        match rhsTerm with 
                        | Term.Value v ->  
                            let  s = valueToString(v)
                            s :: accOp

                        | Term.BinaryOp rhsOp ->                                     
                            serialiseTerm(accOp, Term.BinaryOp rhsOp, thisPrec)
                            
                    | None -> "Error no RHS term" :: acc // this would be an error - we should return a result
                        
            
                match parentPrecedence > thisPrec with 
                | true -> "}" :: accRHS
                | false -> accRHS

                
        serialiseTerm([], term, -1)
        |> List.rev
        |> List.fold (+) ""
                
    let quote(s:string) =
        let sQuote = (string) quot
        (string) sQuote + s + sQuote

    
    let writeStrings(filePath: string) (strings: list<string>) =

        use file = System.IO.File.CreateText(filePath)
        strings |>
        List.iter (fun str ->
            file.WriteLine(str)
        )


    let rec AFAnalysisFromTerm(term: Term): string = 

        // list validated variables, if any

        // walk the tree identifying field inputs - note these will be attributes that are PI Point data references
        // an attribute may be a constant as well as a pi field  - so we would need to signal that in the equation
        // 'sinusoid' +  @'thresholdConstant' - we should be able to get this from the AF database
    
        let rec identifyFields(t: Term, isDivisor: bool, fields: list<string * bool>) =
            // recurse  tree pulling out fields
            match t with 
            | Term.Value  v ->
                match v with 
                | Value.Field field  -> (field, isDivisor) :: fields // for the moment assume that field type is always float  - this could  also be a path - essentially this is either field or pipoint data reference
                | Value.BinaryOpValue bop -> 
                    identifyFields (Term.BinaryOp bop, false, fields)   // recast as BinaryOp and loop again
                | _ -> fields

            | Term.BinaryOp bop -> 
                let fields' = 
                    match bop.LHS with 
                    | Some (lhs, _) -> identifyFields(lhs, false, fields)
                    | None -> fields
                
                match bop.RHS with 
                | Some (rhs, _) -> 
                    
                    let isDivisor' = 
                        match bop.Operator with
                        | Operator (aSym, _prec) ->
                            match aSym with 
                            | Divide -> true
                            | _ -> false
                        | _ -> false

                    identifyFields(rhs, isDivisor', fields')
                | None -> fields'
    
        let getFields(t:Term) =
            identifyFields(t, false, [])
    
    
        let fields =       
            getFields(term)

        let jj =
            fields
            |> List.fold(fun acc (t, _d) ->
                let qq = [
                    sprintf "Var%s := " t;
                    sprintf "\tIf Yearday('t-180m') &lt;&gt; Yearday(PrevEvent('%s', 't')) Then" t;
                    sprintf "\t\tDigState(%s%s" (quote("Calc Failed")) ")";
                    "\tElse";
                    sprintf "\t\tIf BadVal(PrevVal('%s', 't')) Then" t
                    sprintf "\t\tDigState(%s%s" (quote("Calc Failed")) ")";
                    "\tElse";
                    sprintf "\t\tIf BadVal(PrevVal('%s', 't')) Then" t; 
                    sprintf "\t\t\tDigState(%s%s" (quote("Calc Failed")) ")";
                    sprintf "\t\tElse PrevVal(%s, 't')" t; 
                ]
                let s = qq |> String.concat "\r "
                s :: acc
            ) []


        let divisors =
            fields
            |>List.filter(fun (_t, isDivisor) ->
                isDivisor
            )


        let hasDivisors, divisorStr = 
            match divisors with 
            | [] -> false,""
            | _-> 
                let s =
                    divisors
                    |> List.map(fun (t, _d) -> 
                        sprintf "'%s' = 0 " t
                    )
                    |> String.concat " Or "
                true, s

        
        let badVals = 
            fields |> 
            List.map(fun (t, _d) -> 
                sprintf "BadVal('%s')" t
            )
            |> String.concat " Or "
        
        
        let validValues =
            [
                "VarValidValues := ";
                sprintf "\tIf %s Then" badVals;
                sprintf "\t\t0";
                "\tElse";
                sprintf "\t\t1";
            ]

        let calcStr =expressionFromTerm(term)
        let calc = 
            [
                yield "VarCalc := ";
                yield "\tIf VarValidValues = 1 Then"; 
                match hasDivisors with 
                | true ->   
                    let uu = sprintf "\t\tIf %s Then 0" divisorStr                 
                    yield uu
                    yield "\t\tElse"
                    yield sprintf "\t\t\t%s" calcStr
                | false -> 
                    yield sprintf "\t%s" calcStr

                yield sprintf "\tElse DigState(%s%s" (quote("Calc Failed")) ")";
            
            ]
        let clamp = 
            [
                "VariableResult := ";
                sprintf "\tIf BadVal(VariableCalc) Then DigState(%s)" (quote("Calc Failed"));
                "\tElse";
                sprintf "\t\tIf VariableCalc &gt; 'Total|Hi' Then DigState(%s) " (quote("Calc Failed"));
                "\t\tElse";
                "\t\t\tIf VariableCalc &lt; 'Total|Lo' Then 'Total|Lo' ";
                "\t\t\tElse VariableCalc";
            ]
            
        let yy = jj @ validValues @ calc @ clamp

        writeStrings "afAnalysis.txt" yy
        yy |> String.concat "\r"

                
