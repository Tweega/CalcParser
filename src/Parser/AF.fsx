
module ParserTypes =
    open System.Text.RegularExpressions

    type Precedence = int

    type IsAssociative = bool

    type ArithmeticOp = 
    | Plus
    | Minus
    | Multiply
    | Divide
    | NoOp
    | Power
    | Modulo
    
    type  ArithmeticSymbol = 
    | ArithmeticSymbol of ArithmeticOp * IsAssociative

    type ComparatorSymbol = 
    | Equals
    | LessThan
    | GreaterThan
    | LessThanOrEquals
    | GreaterThanOrEquals

    type BinaryOperator = 
    | Operator of ArithmeticOp * Precedence
    | Comparator of ComparatorSymbol
    with 
        member this.OpToString() = 
            match this with 
            | Operator (aSym, _prec) ->
                match aSym with 
                | Plus -> " + "
                | Minus -> " - "
                | Multiply -> " * "
                | Divide -> " / "
                | NoOp -> " NoOp "
                | Power -> " ^ "
                | Modulo -> " % "
            | Comparator cSym -> 
                match cSym with 
                | Equals -> " = "
                | LessThan -> " < "
                | GreaterThan -> " > "
                | LessThanOrEquals -> " <= "
                | GreaterThanOrEquals -> " >= "
        

    //type BinaryOperator = Symbol //* Precedence // precedence only makes sense for arithmetic operators? tk

    type DataType = 
    | Numeric
    | String
    | Boolean
    | Unknown

    // | Boolean?

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
    | Tag of string // for the moment assume that tag type is always float  - this could  also be a path - essentially this is either tag or pipoint data reference
    | Constant of Constant // we could have an option of path here
    | Path of string //we also need to capture if this is a pipoint or not
    | BinaryOpValue of BinaryOp // for bracketed expressions
    | Conditional of TypedTerm * TypedTerm * TypedTerm // Predicate, OnSuccess, OnFail
    | Function of string * list<TypedTerm> // labelled bracketed expression

    and Term = 
    | Value of Value
    | BinaryOp of BinaryOp  // a binaryOp is a monoid and combines two things of the same type

    and TypedTerm = Term * DataType

    [<RequireQualifiedAccessAttribute>]
    type DataQueue = 
    | Input
    | Output

    let noOp = Operator (NoOp, 0)
    let opPlus = Operator (Plus, 1)
    let opMinus = Operator (Minus, 1)
    let opModulo = Operator (Modulo, 1) 
    let opMultiply = Operator (Multiply, 2)
    let opDivide = Operator (Divide, 2)
    let opPower = Operator (Power, 3)


    type OpFunc<'T> = ('T * 'T -> 'T) 
    type CalcOp<'T> = OpFunc<'T> * DataQueue * DataQueue  //make into a record?

    type ParseResult = 
        | ParseOK of option<string> * string //text matching re, remaining string to parse
        | ParseError of string

    type Unary = 
        | Unary of BinaryOperator
        static member Combine(unaryA: Unary, unaryB: Unary) = 
            match (unaryA, unaryB) with
            | Unary (Operator (Plus, _)), Unary (Operator (Plus, _)) -> Ok (Unary opPlus)
            | Unary (Operator (Minus, _)), Unary (Operator (Minus, _)) -> Ok (Unary opPlus)
            | Unary (Operator (Plus, _)), Unary (Operator (Minus, _)) -> Ok (Unary opMinus)
            | Unary (Operator (Minus, _)), Unary (Operator (Plus, _)) -> Ok (Unary opMinus)
            | _ -> Error "Only Plus and Minus accepted as unary operators"

    [<RequireQualifiedAccessAttribute>]
    type Expecting  =
    | BinOp
    | Val of Unary

    type IntegralPart = int
    type FractionPart = int

    let reverseString (input: string) =
        input |> Seq.rev |> Seq.toArray |> System.String



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


    let parseConditional(s:string) =
        let reString: string = @"^if\s+(.+?)\s+then\s+" 
        // let s' = stripLeadingWhitespace(s)
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
                    let resultStr = sprintf "Predicate:(%s) OnSuccess:(%s) OnFail:(%s)" predicate onSuccess onFail

                    ParseOK (Some resultStr, "") // unless we have end if statements, there won't be any remaining - we should have used up the rest of the expression
                | _ ->
                    let msg = sprintf "No else clause in string: %s for predicate %s" s predicate
                    ParseError msg
                    
        | Error msg -> ParseError msg
    

    let expr = "If x > y then if j < k then 33 else 22 else 99"
    let gg = ParserTypes.parseConditional(expr)
