namespace Parser

module CalcParser =
    open ParserTypes
    open System.Text.RegularExpressions
    open Tweega.Utils    
    open Microsoft.FSharp.Core.Operators.Checked
    open System


    type TimeAlias =
    | Yesterday
    | Today
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    with 
        static member getEquivalentDate (now: DateTime) (timeAlias: TimeAlias) =
            let currentDay = now.DayOfWeek // Current day as DayOfWeek enum

            let daysUntil (targetDay: DayOfWeek) =
                // Calculate difference to get back to the target day
                let offset = (int targetDay) - (int currentDay)
                if offset <= 0 then offset - 7 else offset // Go back 1 week if needed

            match timeAlias with
            | Yesterday -> now.Date.AddDays(-1.0) // Midnight yesterday
            | Today -> now.Date                  // Midnight today
            | Monday -> now.Date.AddDays(daysUntil DayOfWeek.Monday |> float)
            | Tuesday -> now.Date.AddDays(daysUntil DayOfWeek.Tuesday |> float)
            | Wednesday -> now.Date.AddDays(daysUntil DayOfWeek.Wednesday |> float)
            | Thursday -> now.Date.AddDays(daysUntil DayOfWeek.Thursday |> float)
            | Friday -> now.Date.AddDays(daysUntil DayOfWeek.Friday |> float)
            | Saturday -> now.Date.AddDays(daysUntil DayOfWeek.Saturday |> float)
            | Sunday -> now.Date.AddDays(daysUntil DayOfWeek.Sunday |> float)


    [<RequireQualifiedAccessAttribute>]
    type TermType =
    | Float of IntegralPart * FractionPart
    | String of string

    let quot = '\u0022'    

    open System.Text.RegularExpressions


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

    let toFloat (value: NumericValue) : Result<float, string> =
        try 
            let i64 = 
                match value with
                | NumericValue.Float64 f -> f
                | NumericValue.Float32 f -> float f
                | NumericValue.Int64 i -> float i
                | NumericValue.Int32 i -> float i
                | NumericValue.Int16 i -> float i
                | NumericValue.Int8 i -> float i
            Ok i64
        with 
            | err -> Error err.Message


    let toInt64 (value: NumericValue) : Result<int64, string> =
        try 
            let i64 = 
                match value with
                | NumericValue.Float64 f -> int64 f //this could throw out of bounds error
                | NumericValue.Float32 f -> int64 f
                | NumericValue.Int64 i -> i
                | NumericValue.Int32 i -> int64 i
                | NumericValue.Int16 i -> int64 i
                | NumericValue.Int8 i -> int64 i
            Ok i64
        with 
            | err -> Error err.Message


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

    let parseDate input =
        // Define the regex pattern
        let pattern =                     
            @"^(0?[1-9]|[12][0-9]|3[01])-" + // Day with optional leading 0
            @"(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-" + // Month
            @"(\d{4})" + // Year
            @"(?:\s(\d{1,2})(?::(\d{2})(?::(\d{2}))?)?)" // Optional time (HH:MM:SS)
        
        let regex = Regex(pattern)
        
        // Match the input string
        let m = regex.Match(input)
        if m.Success then
            // Extract components
            let day = m.Groups.[1].Value
            let month = m.Groups.[2].Value
            let year = m.Groups.[3].Value
            let hour = if m.Groups.[4].Success then m.Groups.[4].Value else "0"
            let minute = if m.Groups.[5].Success then m.Groups.[5].Value else "0"
            let second = if m.Groups.[6].Success then m.Groups.[6].Value else "0"

            let dateStr = (String.Join(":", [day; month; year; hour; minute; second]))
            let remaining = input.Substring(m.Length)

            ParseOK (Some dateStr, remaining)
        else
            let reTimeAlias = "^\s*([Yy]esterday|[Yy]|[Tt]omorrow|[Tt])\b"
            let newValueResult, remaining = reApply(reTimeAlias, input)  
            match newValueResult with 
            | Ok maybeNewValue ->
                ParseOK (maybeNewValue, remaining)
            | Error err ->
                ParseError err

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

    let parseTimeOffset(timeExpr:string) =
        // eg '* + 2d'
        // do we want to parse the expression out at this point or guess that we have a

        // are we allowed floats - or anything that does not convert to int?

        match parseNumber(timeExpr) with 
        | ParseOK (maybeNumber, remaining) ->
            // check if the front of remaining matches a time period

            match maybeNumber with 
            | Some numStr ->
                let reAFDateString: string = sprintf @"^([A-Za-z]+)" // strings are enclosed in double quotes
                match reApply(reAFDateString, remaining) with 
                | Ok (Some afDateStr), remaining' -> 
                    match TimeUnit.toTimeUnit(afDateStr) with 
                    | TimeUnit.Undefined _ -> ParseOK (None, timeExpr)
                    | _tu ->  ParseOK (Some (numStr + ":" + afDateStr), remaining')
                    
                | _ ->
                    ParseOK (None, timeExpr)
                        
            | None -> ParseOK (None, timeExpr)
        | ParseError err -> ParseError err
    
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

    let parseAndHandleTimeOffset(input: string) =
        match parseTimeOffset(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> //e.g. "3:y"
                match (str.Split([|':'|]) |> List.ofArray) with 
                | intStr :: tuStr :: [] ->
                    let ttu = TimeUnit.toTimeUnit tuStr
                    match System.Int32.TryParse(intStr) with 
                        | true, (num:int32) -> 
                            let term = ((Value.TimeOffset (ttu, num))|> Term.Value)
                            Ok (Some (term, DataType.String), remaining)
                        | false, _ -> 
                            Ok (None, input) 

                | _ ->
                    let msg = sprintf "Error in parseAndHandleTimeOffset: Input string does not have the expected format. (%s)"  input
                    Error msg

            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg



    let monthToInt (month:string) =
        match month.ToLower() with
        | "jan" -> 1 | "feb" -> 2 | "mar" -> 3 | "apr" -> 4 | "may" -> 5 | "jun" -> 6
        | "jul" -> 7 | "aug" -> 8 | "sep" -> 9 | "oct" -> 10 | "nov" -> 11 | "dec" -> 12
        | _ -> failwith "Invalid month"


    let parseAndHandleFixedDate(input: string) =
        match parseDate(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                match (str.Split([|':'|]) |> List.ofArray) with 
                | day :: month :: year :: hours :: minutes :: seconds :: [] ->
                    try
                        let dayInt = int day
                        let monthInt = monthToInt month
                        let yearInt = int year
                        let hourInt = int hours
                        let minuteInt = int minutes
                        let secondInt = int seconds

                        let dateTime = DateTime(yearInt, monthInt, dayInt, hourInt, minuteInt, secondInt)
                        let term = dateTime |> (Value.FixedDate >> Term.Value) 
                        Ok (Some (term, DataType.DateTime), remaining)
                    with
                    |  :? FormatException as err -> 
                        Error err.Message
                    | :? ArgumentOutOfRangeException as err->
                        Error err.Message
                | _ ->
                    let msg = sprintf "Error in parseAndHandleFixedDate: Input string does not have the expected format. (%s)"  input
                    Error msg
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    let parseTimeUnit (input: string) =
        let reTimeUnit = @"^\s*(y|mo|w|d|h|m|s)\b"
        let newValueResult, remaining = reApply(reTimeUnit, input)  
        match newValueResult with 
        | Ok maybeNewValue ->
            ParseOK (maybeNewValue, remaining)
        | Error err ->
            ParseError err
        

    let parseAndHandleTimeUnit(input: string) =
        // we first need an integer (not sure if we need to handle floats)
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some intStr -> 
                match System.Int32.TryParse(intStr) with 
                | true, (n:int32) -> 
                    match parseTimeUnit(remaining) with 
                    | ParseOK (maybeMatch, remaining') -> 
                        match maybeMatch with 
                        | Some tu ->
                            let maybeTU = 
                                match tu.ToLower() with
                                | "y"  -> Some TimeUnit.Year
                                | "mo" -> Some TimeUnit.Month
                                | "w"  -> Some TimeUnit.Week
                                | "d"  -> Some TimeUnit.Day
                                | "h"  -> Some TimeUnit.Hour
                                | "m"  -> Some TimeUnit.Minute
                                | "s"  -> Some TimeUnit.Second
                                | _    -> None // Catch-all for unexpected cases
                            
                            match maybeTU with 
                            | Some timeUnit -> 
                                let term = (timeUnit, n) |> (Value.TimeOffset >> Term.Value)
                                Ok (Some (term, DataType.DateOffset), remaining')
                            | None -> Ok (None, input)
                            
                        | _ -> Ok (None, input)
                    | ParseError err ->
                        Error err
                | false, _ ->  Ok (None, input) // expression does not start with a number
            | None -> Ok (None, input)
        | ParseError err -> Error err


    let parseAndHandleNumber(input: string) =
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> (NumericalConst >> Value.Constant >> Term.Value)
                // assume that all numbers are float64 for the moment 0 this needs to change tk
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

    let rec parseExpression(dataTypeMap:Map<string, DataType>) (expr: string) =
        // let rootOp = makeRootOp()
        // let opStack: Stack<BinaryOp> = Stack []
        // let rootStack = Stack.push rootOp opStack
        // this will be passed in but hard code while debugging
        
        // put these into a wrapper function so we don't have to keep re-assigning
        let parseAndHandleFieldOrTime' = parseAndHandleFieldOrTime dataTypeMap
        let parseAndHandleConditional' = parseAndHandleConditional dataTypeMap
        let parseAndHandleBrackets' = parseAndHandleBrackets dataTypeMap
        let parseAndHandleFunction' = parseAndHandleFunction dataTypeMap

        let parseAndHandleValue = parseAndHandleFieldOrTime' >=> parseAndHandleNumber >=> parseAndHandleString >=> parseAndHandleBrackets'
        let parseAndHandleTerm = parseAndHandleValue >=> parseAndHandleBinaryOperator >=> parseAndHandleFunction' >=> parseAndHandleConditional'

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

    // keep Fields and Time separate?  We have them together for now because they are both wrapped in single quotes
    and parseAndHandleFieldOrTime(dataTypeMap: Map<string, DataType>) (input: string) =
        match parseField(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let maybeDT = Map.tryFind (str.ToUpper()) dataTypeMap
                match maybeDT with 
                | Some dt ->
                    let term = str |> ((Value.Field >> Term.Value))
                    Ok (Some (term, dt), remaining) 
                | None -> 
                    // check if we have a time expression
                    // this requires that parseExpression can parse date strings
                    // can we pass in the parser
                    // WORKING HERE
                    match parseExpression Map.empty str with 
                    | Ok (typedTerm) -> 
                        Ok (Some typedTerm, remaining)
                    | Error _err ->
                        let msg = sprintf "Unable to resolve tag or time expression for %s" str
                        Error msg
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    
    and parseAndHandleBrackets(dtMap: Map<string,DataType>)(input: string) =
        match parseBrackets(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let termRes = parseExpression dtMap str
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
    
    and parseAndHandleFunction(dtMap: Map<string,DataType>)(input: string) 
        : Result<option<TypedTerm> * string,string> =
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
                            let f = (Term.Value (Value.Function (funcName, DataType.Unknown, [])), DataType.Numeric Number.Float64)
                            Ok (Some f, remaining')
                        |true -> 
                            let parameters = parameterString.Split ','
                            // create a root binary operator for each parameter
                            printfn "Not expecting to get here"
                            // parse each parameter expression
                            let argResults = 
                                parameters |> 
                                List.ofArray |>
                                List.map (parseExpression dtMap) |>
                                List.rev                            
                                
                            // now run through the results of each paramter from list<results>
                            // | Function of string * list<TypedTerm> // labelled bracketed expression
                            
                            let fTermsRes =
                                argResults |>
                                List.fold(fun (acc: Result<list<TypedValue>, string>) (res:Result<(Term * DataType),string>) ->
                                    match acc with 
                                    | Ok tts ->
                                        match res with 
                                        | Ok (t, dt) ->
                                            match t with 
                                            | Term.BinaryOp bop -> 
                                                // I will eventually need an evaluator for each function
                                                // and each evaluator will either have its own list of inputs
                                                // or take from a common one
                                                // how would I know the data type of an argument. 
                                                // we have dt but where did we get that from?
                                                Ok ((bop |> Value.BinaryOpValue, dt ) :: tts)
                                            | Term.Value v -> 
                                                Ok ((v, dt) :: tts)
                                        | Error msg -> Error msg
                                    | Error msg -> Error msg
                                ) (Ok [])

                                // Ok (Some (term, DataType.Numeric), remaining) // assume that attributes return numeric values for the moment
                            match fTermsRes with 
                            | Ok tvs -> 
                                // not sure how we will know the return type  of a function unless it is registered in some way
                                // this will either be supplied later after a lookup of functin name 
                                // or a map of functions needs to be passed in here for the lookup to be done now tk
                                
                                let returnType = DataType.Numeric Number.Float64
                                let typedTerm = (Term.Value (Value.Function (funcName, returnType, tvs)), returnType)
                                Ok (Some typedTerm, remaining')
                            | Error msg -> Error msg

                    | None -> 
                        Ok (None, input)

                | ParseError msg -> Error msg

        | ParseError msg ->
            Error msg

    and parseAndHandleConditional(dtMap: Map<string,DataType>)(input: string) =
        match parseConditional(input) with 
        | ParseOK (cond, remaining) -> 
            match cond with
            | None -> 
                Ok (None, input)
            | Some conditional ->
                let parts = conditional.Split [|':'|] |> List.ofArray
                match parts with 
                | predicate :: success :: fail :: [] ->
                    let predicateResult = parseExpression dtMap predicate
                    let successResult = parseExpression dtMap success 
                    let failResult = parseExpression dtMap fail
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


    // this assumes that you can't have a nested time expression, whatever that might look like.
    // or rather you can have a nested time expression, only that can't contain tags or reference earlier variables 
    // you might then have '(1 + 2)d' - it would be difficult to see how you could have nested quotes
    // you might be able to do it via a conditional expression but that would require being able to 
    // resolve tags from the expression, and why can't we have nested quotes
    // we would then have the if then end if problem - where is the final qupte
    // we don't distinguish between beginning and end as we do with brackets
    // so unless we change the synatax, we can't have nested
    // this would mean that you could have binary operator follwed by time unit
    // we would need to parse this differently then as the d would not follow on from a number, but a
    // binary operator

    // we could possibly be able to use conditionals to return time strings that in turn would be parsed
    // but that might mean parsing on the fly unless we could store evaluators for the success/fail branches
    // for the moment assume nothing nested, then allow brackets and conditionals
    // any inputs for conditionals would have to be supplied by the caller
    and parseTimeExpression
        (parser:string -> Result<Option<TypedTerm> * string,string>)
        (dataTypeMap:Map<string, DataType>) 
        (expr: string) =

            let parseAndHandleTerm = parseAndHandleFixedDate >=> parseAndHandleNumber >=> parseAndHandleBinaryOperator
            parseExpression Map.empty expr

    let getQueueType(t: Term) = 
        match t with 
        | Term.Value v-> 
            match v with 
            | Value.BinaryOpValue _bop -> QueueType.Output
            | Value.Constant c -> QueueType.Constant c
            | _-> QueueType.Input
        | Term.BinaryOp _bop -> QueueType.Output


    let rec processCalcTree<'T>(
        (term, dt): TypedTerm, 
        inputs: list<Value * DataType>, 
        calcOps:list<BinaryCalcOp>) =

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
                processCalcTree((Term.BinaryOp bop, dt), inputs, calcOps)
            | Value.Constant _c -> 
                inputs, calcOps
            | _ ->
                (v, dt) :: inputs, calcOps

        | Term.BinaryOp bop ->             
            match bop.LHS, bop.RHS with 
            | Some (lhsTerm, lhsDT), Some (rhsTerm, rhsDT) -> 
                let lhsQueueType = getQueueType(lhsTerm)    // either an input from 'user' or an output from a sub calculation               
                let rhsQueueType = getQueueType(rhsTerm)
                
                let funcImpl =  Map.find bop.Operator // we could check here that lhsDT and rhsDT are of the same type
                let calcOps' = (bop.Operator, lhsQueueType, rhsQueueType) :: calcOps

                // if lhs is a value, need to lay that down before processing rhs, unless rhs is a value in which case it goes down first
                match rhsTerm with 
                | Term.BinaryOp _rBop -> 
                    printfn "rhs term is binary"

                    match lhsTerm with
                    | Term.BinaryOp _lBop -> 
                        // these are both binary ops - process the rhs first
                        printfn "lhs term is binary"
                        let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps')
                        processCalcTree((lhsTerm, lhsDT), inputs', calcOps'')
                    | Term.Value v ->
                        printfn "lhs term is value"

                        match v with 
                        | Value.BinaryOpValue _bopV -> 
                            printfn "lhs term is binary VALUE"
                            // this is actually a binary operator so process rhs first
                            let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps')
                            processCalcTree((lhsTerm, lhsDT), inputs', calcOps'')
                        | _ ->  
                            // lay lhs value down in inputs before processing rhs
                            printfn "Are we getting  here??"
                            let (inputs', calcOps'') = processCalcTree((lhsTerm, lhsDT), inputs, calcOps')
                            processCalcTree((rhsTerm, rhsDT), inputs', calcOps'')
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
                            let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps')
                            processCalcTree((lhsTerm, lhsDT), inputs', calcOps'')
                        | Term.Value v ->
                            printfn "lhs term is value"

                            match v with 
                            | Value.BinaryOpValue _bopV -> 
                                printfn "lhs term is binary VALUE"
                                // this is actually a binary operator so process rhs first
                                let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps')
                                processCalcTree((lhsTerm, lhsDT), inputs', calcOps'')
                            | _ ->  
                                // lay lhs value down in inputs before processing rhs
                                printfn "Are we getting  here??"
                                let (inputs', operators'') = processCalcTree((lhsTerm, lhsDT), inputs, calcOps')
                                processCalcTree((rhsTerm, rhsDT), inputs', operators'')
                    | _ ->
                        // process the rhs first regardless of lhs
                        let (inputs', calcOps'') = processCalcTree((rhsTerm, rhsDT), inputs, calcOps')
                        processCalcTree((lhsTerm, lhsDT), inputs', calcOps'')
                    

            | _ -> 
                // we will have to wrap this up in a Result, but for now log and drop out
                // alternatively we could prevalidate and work on validated structures
                printfn "Error:  Need LHS and RHS in Binary Operator %A" bop
                inputs, calcOps

    let int64Plus(a:int64, b:int64) = 
        try 
            let i64 = a + b
            ResolvedValue.Numeric (NumericValue.Int64 i64)
        with 
            | err -> ResolvedValue.BadVal err.Message

    let int64Minus(a:int64, b:int64) = 
        try 
            ResolvedValue.Numeric (NumericValue.Int64 (a - b))
        with 
            | err -> ResolvedValue.BadVal err.Message

    let int64Multiply(a:int64, b:int64) = 
        try 
            ResolvedValue.Numeric (NumericValue.Int64 (a * b))
        with 
            | err -> ResolvedValue.BadVal err.Message


    let int64Divide(a:int64, b:int64) = 
        try 
            ResolvedValue.Numeric (NumericValue.Int64 (a / b))
        with 
            | err -> ResolvedValue.BadVal err.Message

    let int64Modulo(a:int64, b:int64) = 
        try 
            ResolvedValue.Numeric (NumericValue.Int64 (a % b))
        with 
            | err -> ResolvedValue.BadVal err.Message

    let floatPlus(precision:Number, a:float, b:float) = 
        try 
            let f = a + b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message

    let floatMinus(precision:Number, a:float, b:float) = 
        try 
            let f = a - b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message

    let floatMultiply(precision:Number, a:float, b:float) = 
        try 
            let f = a * b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message
    
    let floatDivide(precision:Number, a:float, b:float) = 
        try 
            let f = a / b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message

    let floatPower(precision:Number, a:float, b:float) = 
        try 
            let f = a ** b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message

    let floatModulo(precision:Number, a:float, b:float) = 
        try 
            let f = a / b
            ResolvedValue.Numeric <| castToOriginalPrecision f precision 
        with 
            | err -> ResolvedValue.BadVal err.Message

    
    let evaluateBinaryOp (bop:BinaryOperator, rv1: ResolvedValue, rv2:ResolvedValue) =
        match (rv1, rv2) with 
        | ResolvedValue.Numeric a, ResolvedValue.Numeric b -> 
            let precision = determinePrecision a b
            match precision with 
            | Number.Int64 ->
                match ((toInt64 a), (toInt64 b)) with 
                | Ok int64A, Ok int64B  ->
                    match bop with 
                    | BinaryOperator.Operator (op, _) ->
                        match op with
                        | Plus -> int64Plus(int64A, int64B)
                        | Minus -> int64Minus(int64A, int64B)
                        | Multiply -> int64Multiply(int64A, int64B)
                        | Divide -> int64Divide(int64A, int64B)
                        | NoOp -> ResolvedValue.BadVal "NoOp encountered"
                        | Power -> 
                            match (toFloat a), (toFloat b) with
                            | Ok floatA, Ok floatB -> 
                                floatPower(precision, floatA, floatB) //int64 does not support power operator so try with float and hope for the best
                            | _, Error err -> ResolvedValue.BadVal err
                            | Error err, _ -> ResolvedValue.BadVal err
                    
                        | Modulo -> int64Modulo(int64A, int64B)

                    | BinaryOperator.Comparator (_sym) -> ResolvedValue.BadVal "Not implemented yet"

                | _, Error err -> ResolvedValue.BadVal err
                | Error err, _ -> ResolvedValue.BadVal err
            | _ ->
                match ((toFloat a), (toFloat b)) with 
                | Ok floatA, Ok floatB  ->
                    match bop with 
                    | BinaryOperator.Operator (op, _) ->
                        match op with    
                        | Plus -> floatPlus(precision, floatA, floatB)
                        | Minus -> floatMinus(precision, floatA, floatB)
                        | Multiply -> floatMultiply(precision, floatA, floatB)
                        | Divide -> floatDivide(precision, floatA, floatB)
                        | NoOp -> ResolvedValue.BadVal "NoOp encountered"
                        | Power -> floatPower(precision, floatA, floatB)
                        | Modulo -> floatModulo(precision, floatA, floatB)
                    | BinaryOperator.Comparator (_sym) -> ResolvedValue.BadVal "BinaryOperator.Comparator Not implemented yet"
                | _, Error err -> ResolvedValue.BadVal err
                | Error err, _ -> ResolvedValue.BadVal err

        | ResolvedValue.String a, ResolvedValue.String b -> 
            ResolvedValue.String (sprintf("%s%s") a b)

        | ResolvedValue.FixedDate _rd, ResolvedValue.DateOffset (_qty: int, _tu: TimeUnit) ->
            // idea here would be to cast the date to ticks, then resolve the dateoffset to ticks
            // do the arithmetic and cast back to a Relative date with no offset - which should then be an option
            // either that or to store the result as a int64, in which case we need to match on
            // Int64, DateOffset as well - perhaps relative date could be an Int64
            // except that relative date might be "t" and it might be easier to resolve that here
            ResolvedValue.BadVal "Time arithmetic not implemented yet"

        | ResolvedValue.DateOffset (_int1, _tu1), ResolvedValue.DateOffset (_int2, _tu2) ->
        // idea here would be to cast the date to ticks, then resolve the dateoffset to ticks
        // do the arithmetic and cast back to a Relative date with no offset - which should then be an option
        // either that or to store the result as a int64, in which case we need to match on
        // Int64, DateOffset as well - perhaps relative date could be an Int64
        // except that relative date might be "t" and it might be easier to resolve that here
            ResolvedValue.BadVal "Time arithmetic not implemented yet"

        | ResolvedValue.BadVal "Time arithmetic not implemented yet", _
        
        |_ ->
            let msg = sprintf "Invalid types for operator plus.  Got (%s, %s)" (rv1.ToString()) (rv2.ToString())
            ResolvedValue.BadVal msg
    
        // printfn "adding %f, %f" a b
        // a + b

    
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

    let getResolvedValue(dataQ:QueueType, inputs: list<ResolvedValue>, outputs: list<ResolvedValue>) = 
        // should we return a BadValue instead of None? tk
        match dataQ with 
        | QueueType.Input ->
            match inputs with 
            | [] -> 
                let msg = "No value in inputs"
                Error msg, inputs, outputs
            | h :: t -> (Ok h, t, outputs)
        | QueueType.Output ->
            match outputs with 
            | [] -> 
                let msg = "No value in outputs"
                Error msg, inputs, outputs
            | h :: t -> Ok h, inputs, t
        | QueueType.Constant c ->   //for constants we don't take off either input stack (outputs are inputs that result from binary operations)
            match c with 
            | Constant.NumericalConst nStr ->
                let maybeNumerical = tryResolveNumber nStr
                match maybeNumerical with 
                | Some numerical -> 
                    Ok (ResolvedValue.Numeric numerical), inputs, outputs
                | None -> 
                    let msg = sprintf "Unable to resolve numerical constant %s" nStr
                    Error msg, inputs, outputs
            | Constant.StringConst str -> 
                Ok (ResolvedValue.String str), inputs, outputs

    // creates a single function from a sequenced list of binary operations which have been ordered in the parsing process
    // so that dependencies are executed first and interim values put on an output stack from where they can
    // be consumed, along with the input stack passed in
    let createBinOpEvaluator<'T>(ops:list<BinaryCalcOp>) 
        : list<ResolvedValue> -> Result<ResolvedValue,string> =
        // pass in number of args? tk
        // how will this work with functions, where the inputs may have different types? tk
        // ideally functions will not have to unbox all their inputs, but that might be the only way to do it
        fun(inputs: list<ResolvedValue>) ->
            // validate number of inputs?
            // iterate through each calcOp and pass it the inputs and outputs lists
            // let inputsRev = List.rev inputs
            let outputs: list<ResolvedValue> = []
            let (ins, outs) =
                ops |>
                List.fold(fun (inAcc, outAcc) (bop, lhsQ, rhsQ) -> 
                    let (lhsVal, inputsLHS: list<ResolvedValue>,  outputsLHS: list<ResolvedValue>) = 
                        getResolvedValue(lhsQ, inAcc, outAcc)
                    let (rhsVal, inputsRHS: list<ResolvedValue>,  outputsRHS: list<ResolvedValue>) = 
                        getResolvedValue(rhsQ, inputsLHS, outputsLHS)
                    match (lhsVal, rhsVal) with 
                    | (Ok lhs, Ok rhs) ->
                        //let t:ResolvedValue = mappend(lhs, rhs) //mappend calls the plus/minus etc function
                        let t:ResolvedValue = evaluateBinaryOp(bop, lhs, rhs)
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

    let timeServer(timeNow: System.DateTime)(timeExpression:string) = 
        let timeT = timeNow
        let timeY = timeNow

        // check for fully qualified time
        // check for time addition
        let jj = parseExpression Map.empty timeExpression
        // the expression coming in could be of arbitrary length
        // so we need to parse it as a binary op tree.

        jj

    let tagMax(inputs: list<ResolvedValue>) =
        // functions should not be taking output of other boolean operations?
        // tagMax takes 3 arguments
        match inputs with 
        | _tagName :: _startTime :: _endTime :: [] ->
            // time parameters need parsing to date times relative to some Now
            // unless times have already been parsed.
            // the (decisive) advantage of not parsing them until now
            // is that they can be passed as is to AF if we have access to that.
            // this function would then construct and call the command to interrogate AF

            Ok (ResolvedValue.Numeric (NumericValue.Float64 12.3))
        | _ -> Error "Wrong number of inputs in tagMax"



    let createFunctionEvaluator(functionName:FunctionName, functionArgs:list<FunctionArg>)
        : list<ResolvedValue> -> Result<ResolvedValue,string> =


        // somewhere there will be a map from which I can get details for functionName.
        // function args are either constants - is a tag name a constant? 
        // or they are InputValues - either a Field or a Function - tag is a field
        // as things stand

        // simulate a lookup of a function - say on
        let functionToUse = tagMax

        functionToUse

    let testParseExpression(expr:string) = 
        let dtMap = 
            [
                ("SINUSOID", (DataType.Numeric Number.Float64))
                ("CDT158", (DataType.Numeric Number.Float64))
            ]
            |> Map.ofList
        let exprRes = parseExpression dtMap expr // these values are just placeholders though they are returned from processCalcTree

        match exprRes with 
        | Error msg -> 
                Error (sprintf "Error: %s" msg)
        | Ok binOp -> 
            printfn "binOp:\n%A" binOp
            // perhaps operators need not be part of this map - it is hard to imagine how their implementations might change
            
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

            //let joinedLists = List.Join([comparatorOps; numericOps])
            //let opMap = joinedLists |> Map.ofList

            let (inputs: list<Value * DataType>), (operators: list<BinaryCalcOp>) = processCalcTree(binOp, [], [])
            
            printfn "%A" inputs
            printfn "%A" operators
            
            let evaluator = createBinOpEvaluator(operators)

            // map inputs (Typed Values) to input values
            let rec tryMapTypedValuesToInputValues(ins: list<TypedValue>)  =
                ins |> 
                List.fold(fun acc (v, dt) -> 
                    match acc with 
                    | Ok acc' ->
                        match v with 
                        | Value.Field fieldName ->  
                            let yy = Ok ((((fieldName, dt) |> InputValue.Field)) :: acc')
                            yy
                        | Value.Function (funName, dt, tvs) ->
                            let funArgsResult: Result<list<InputValue>, string> = 
                                tryMapTypedValuesToInputValues(tvs)
                                |> Result.bind(fun inVals -> 
                                    //wrap each input value in FunctionArg.InputValue
                                    inVals |> List.map FunctionArg.InputValue |> Ok
                                )
                                |> Result.bind(fun inputArgs -> 
                                    Ok (InputValue.Function (funName, dt, inputArgs) :: acc')
                                )
                            funArgsResult                                    
                            
                        | _ -> 
                            let msg = sprintf "Invalid value type in final list of values: %A" v
                            Error msg
                    | Error err -> Error err 
                    
                ) (Ok list<InputValue>.Empty)
                

            match (tryMapTypedValuesToInputValues inputs) with 
            | Ok inputValues -> 
                Ok (inputValues, evaluator)
            | Error err -> Error err
            
            
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

            | Value.Function (fName, _, _) -> sprintf "Function: %s" fName
            | Value.Conditional c -> sprintf "Conditional: %s" (c.Predicate.ToString())
            | Value.FixedDate c -> sprintf "FixedDate: %s" (c.ToString())
            | Value.TimeOffset (a,c) -> sprintf "Time Offset: %s:%s" (a.ToString()) (c.ToString())
        
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

                
