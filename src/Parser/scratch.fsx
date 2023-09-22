
    #r @".\bin\Debug\net6.0\Parser.dll"
    open Parser.ParserTypes
    open System.Text.RegularExpressions

    let (|Exists|_|) = Map.tryFind

    let (|IsTruex|_|) pred x =
        if pred x then Some () else None

    let (|IsTrue|_|)  x =
        if x = true then Some () else None

    
    let (|Eq|_|) expected value =
        match expected = value with 
        | true -> Some ()
        | _ -> None 

    [<RequireQualifiedAccessAttribute>]
    type DataType =
    | Float
    | Integer
    | String

    [<RequireQualifiedAccessAttribute>]
    type ComparisonOp =
    | GT
    | LT
    | LTE
    | GTE
    | EQ
    
    [<RequireQualifiedAccessAttribute>]
    type Axis =
    | Self
    | Child
    | Parent
    | Ancestor
    | Descendant
    | DescendantOrSelf
    | AncestorOrSelf
    | Global

    [<RequireQualifiedAccessAttribute>]
    type NodeType =
    | Element
    | Attribute

    type FunctionName = string

    [<RequireQualifiedAccessAttribute>]
    type Comparator = 
    | ComparisonOp of ComparisonOp
    | Function of FunctionName

    [<RequireQualifiedAccessAttribute>]
    type NodeName =
    | NodeName of string
    | Any

    [<RequireQualifiedAccessAttribute>]
    type TermType =
    | Float of IntegralPart * FractionPart
    | String of string

     type NodeSelection = {
        Name: string;
        Type: NodeType;
    }

    type ConstantValue = {
        DataType:DataType;
        Value: string;
    }

    type BinaryComparison = {
        LHS: FilterExpression;
        RHS: FilterExpression;
        ComparisonOp: ComparisonOp;
    } 
    and Filter = 
    | BinaryComparison of BinaryComparison
    | Function of string * FilterExpression * FilterExpression
    
    and FilterExpression = 
    | Expression of Expression //nodeList
    | Constant of ConstantValue
   
    and Expression =  //expression  evaluates to a node list
        | NodeListExpression of Axis * list<Filter> * JutzPath * NodeSelection //Axis, Filters, Continuation, NodeSelection;

    and JutzPath =
    | Expression
    | Empty

    // we could add starting position in the original parse string to indicate where failure is tk
    [<RequireQualifiedAccessAttribute>]
    type JPTerm =
    | Axis of Axis
    | FilterExpression of FilterExpression
    | NodeSelection of NodeSelection
    | NodeName of NodeName
    | Filter of list<JPTerm>
    | Function of string * list<JPTerm>

    // some axes identify nodes already such as parent and . so we don't need a NodeSelection
    // descendants/* -> the /* is redundant but functions as an element filter
    let xPath = "./station/pump1/@pressure"

    // Expression scope
        // axis -> NodeSelection -> maybeFilters -> axis
        // first  MUST come axis
        // second MUST comeNodeSelection
        // third optionally filters
    
    //Filter scope []
        // one of
            // function name then expression (axis/node  selection etc)
            // FilterExpression

    // FilterExpression scope
        //one of
            //constant
            //Expression

    //COnstantscope
        //one of 
        // int, float, string


    let doubleQuote = '\u0022'    
    let singleQuote = '\u0039'    
    
    let composeParsers(f1: string -> Result<Option<list<JPTerm>> * string, string>) (f2: string -> Result<option<list<JPTerm>> * string, string>) =
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
        printfn "reApply has received [%s:%s]" s re
        let rx = Regex(re, RegexOptions.IgnoreCase + RegexOptions.Multiline +  RegexOptions.Compiled)
        let m = rx.Match(s)

        match m.Success with 
        | true -> 
            let (matchResult, newS) = 
                match m.Captures.Count with
                | 1 ->  // working here on whitespace issue.  we may need to match on whitespace separately
                    printfn "We have a match: %A %d" m.Captures[0].Value m.Length
                    printfn "We have a match: %A %d" m.Groups[0].Value m.Groups.Count
                    printfn "We have a match: %A %d" m.Groups[1].Value m.Groups.Count
                    (Ok (Some m.Groups[1].Value), s[m.Groups[1].Value.Length ..])
                | _ -> 
                    let msg = sprintf "More than one group matched in reg exp: %s on string: %s" re s
                    (Error msg), s

            matchResult, newS    

        | false -> 
            printfn "no match: %s :%s " re s
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
                    let illegalChars = sprintf "%s%c" @"*'\?;{}[\]\|\\`\" doubleQuote //we wouldn't actually detect single quote here
                    let reIllegal = sprintf "[%s]" illegalChars
                    match reApply(reIllegal, tag) with 
                    | Ok (Some x),_s -> 
                        let msg = sprintf "Illegal character (%s)in tag name: %s" x s
                        ParseError msg
                    | _ ->
                        let reNonPrintable = @"[^ -~]"
                        match reApply(reNonPrintable, tag) with 
                        | Ok (Some _), _s -> 
                            let msg = sprintf "Non printable character in tag name: %s" s
                            ParseError msg
                        | _ ->
                            ParseOK (maybeTag, remaining)
                    
        | Error msg -> ParseError msg
        

    let parseString(quot:char)(s:string) =
        let reString: string = sprintf @"^\%c(.+)\%c" quot quot
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

    let parseQuotedString = parseString singleQuote
    let parseDoubleQuotedString = parseString doubleQuote
        
    let parseIfThenElse() = 
        // to be done. this re gets between if and end on a multiline input
        // after which we would want to separate out on then and else
        //the first clause would split on an equality operator
        // perhapsif  the else  clause is  not given then default to noOutput.

        let re = @"^\s*if \s*([^-~]+)\s*end"
        re


    let parseFunctionName(s:string) =
    //^\((.+)\)
        let reString: string = sprintf @"^([A-Za-z0-9]+)\s*\(" //function name starts with alpha optionally continues with alphaNum and terminates with open parenthesis
        let newValueResult, remaining = reApplyX(reString, s)
        match newValueResult with 
        | Ok maybeFuncName -> 
        
            match maybeFuncName with 
            | None ->ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeFuncName, remaining)
                    
        | Error msg -> ParseError msg

    let parseFunctionContents(s:string) =
    //^\((.+)\)
        let reString: string = sprintf @"^\((.+)\)"
        let newValueResult, remaining = reApplyX(reString, s)
        match newValueResult with 
        | Ok maybeFuncName -> 
        
            match maybeFuncName with 
            | None ->ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeFuncName, remaining[1 .. remaining.Length -  2]) //strip off the brackets from the where clause
                    
        | Error msg -> ParseError msg

    let parseElementSelection(s:string) =
        let reElement: string = sprintf @"^([A-Za-z0-9]+)"
        
        let newValueResult, remaining = reApplyX(reElement, s)
        match newValueResult with 
        | Ok maybeNodeSelection -> 
        
            match maybeNodeSelection with 
            | None -> ParseOK (None, s)

            | Some str ->
                ParseOK (maybeNodeSelection, remaining)
                    
        | Error msg -> ParseError msg

    let parseAttributeSelection(s:string) =
        let reAttribute: string = sprintf @"^(@[A-Za-z0-9]+)"
        
        let newValueResult, remaining = reApplyX(reAttribute, s)
        match newValueResult with 
        | Ok maybeNodeSelection -> 
        
            match maybeNodeSelection with 
            | None -> ParseOK (None, s)

            | Some str ->
                let attrName = str[1..]
                ParseOK (Some attrName, remaining)
                    
        | Error msg -> ParseError msg

    
    let parseWhere(s:string) =
        let reWhere: string = sprintf @"^\[(.+)]"
        
        let newValueResult, remaining = reApplyX(reWhere, s)
        match newValueResult with 
        | Ok maybeWhere -> 
        
            match maybeWhere with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeWhere, remaining[1 .. remaining.Length -  2]) //strip off the brackets from the where clause
                    
        | Error msg -> ParseError msg

        
    let parseAxis(s:string) =
        let reAxis: string = sprintf @"^([A-Za-z0-9\.]*\/+)."  //includes trailing slashes
        
        let newValueResult, remaining = reApplyX(reAxis, s)
        match newValueResult with 
        | Ok maybeAxis -> 
        
            match maybeAxis with 
            | None -> ParseOK (None, s)

            | Some str ->
                ParseOK (maybeAxis, remaining)
                    
        | Error msg -> ParseError msg
    
    let parseAxisLong(s:string) =
        let reAxis: string = sprintf @"^([A-Za-z0-9]+)::"  //includes trailing slashes
        
        let newValueResult, remaining = reApplyX(reAxis, s)
        match newValueResult with 
        | Ok maybeAxis -> 
        
            match maybeAxis with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeAxis, remaining[2..])//skip the :: characters 
                    
        | Error msg -> ParseError msg
    
        
    let parseBrackets(bracketChars: char * char)(s:string) =
        // we could replace all the reg exps with character parsing except that would probably look  more like the voice analyser
        let openBracketChar = fst(bracketChars)
        let closeBracketChar = snd(bracketChars)

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
            | c :: t -> 
                match c with
                | Eq openBracketChar ->
                    match bracketCount with 
                    | 0 -> processString(t, bracketCount + 1, acc) // don't capture the first opening bracket
                    | _ -> processString(t, bracketCount + 1, '(' :: acc) // capture internal brackets
                
                | Eq closeBracketChar ->
                    match bracketCount with 
                    | 0 -> 
                        let msg = "close bracket before open bracket"
                        ParseError msg
                    | 1 -> 
                        let str = System.String.Concat(Array.ofList(List.rev acc))
                        let remaining = System.String.Concat(Array.ofList(t))
                        ParseOK (Some str, remaining) 
                    | _ -> processString(t, bracketCount - 1, ')' :: acc)
                
                | _ ->
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

    let parseRoundBrackets = parseBrackets('(',')')
    let parseSquareBrackets = parseBrackets('[',']')
    let parseSquigglyBrackets = parseBrackets('{','}')

    let parseOperator(s:string) =
        let reOp = @"^([+-\/\*\^%])"
        let newValueResult, remaining = reApplyX(reOp, s)      
        match newValueResult with 
        | Ok maybeOperator ->
            ParseOK (maybeOperator, remaining)
        | Error msg -> ParseError msg

//----------
    // we need handlers to return a consistent data type representing the document / data structure we are building
    // we are creating a tree, alhough we could at this point simply gather terms as we do when parsing function expressions
    // and then assemble the terms in a final step
    // gatherTerms then will produce a list<JPTerm>
    // each handler function will return a list<JPTerm> to be flattened   into accumulaor list<JPTerm>
    let getAxis(axisStr:string) =
        
        match axisStr.ToLower() with
        | "." -> Some Axis.Self
        | ".." -> Some Axis.Parent
        | "self" -> Some Axis.Self
        | @"/" -> Some Axis.Child
        | "child" -> Some Axis.Child
        | "descendant" -> Some Axis.Descendant
        | "ancestor" -> Some Axis.Ancestor
        | "parent" -> Some Axis.Parent
        | "descendant-or-self" -> Some Axis.DescendantOrSelf
        | "ancestor-or-self" -> Some Axis.AncestorOrSelf
        | "" -> Some Axis.Global
        | _ -> None

    let parseAndHandleAxes(input: string) : Result<option<list<JPTerm>> * string, string> =
        match parseAxisLong(input) with 
        | ParseError msg ->
            Error msg

        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | None -> 
                //long did not work, try short
                match parseAxis(input) with 
                | ParseError msg ->
                    Error msg

                | ParseOK (maybeMatch, remaining') -> 
                    match maybeMatch with 
                    | None -> 
                        Ok (None, input)  //no error but we don't have an axis
                    | Some str -> 
                        // Ok (Some str, remaining')
                        let firstSlashPos = str.IndexOf('/')
                        let n = str.Length - firstSlashPos - 1
                        printfn "n is %d, firstSlashPos is %d strLen is %d" n firstSlashPos str.Length
                        let axisStr = str[0..firstSlashPos - 1]
                        printfn "Axis string is %s" axisStr
                        let axis = getAxis(axisStr)
                        match axis with
                        | Some axis -> 
                            let termA = (JPTerm.Axis axis)
                            match n with 
                            | Eq 0 ->
                                // we have a single axis, such as .
                                Ok (Some [termA], remaining')
                                
                            | Eq 1 ->
                                // double slash following an axis - this is 2 axes, such as parent//, the one identified plus all descendants
                                // in this case we also need to return a selection of Any
                                let termC = (JPTerm.Axis Axis.Descendant)
                                match axis with 
                                | Axis.Global ->
                                    Ok (Some [termA], remaining)
                                | _ ->

                                    let termB = (JPTerm.NodeName NodeName.Any)
                                    Ok (Some [termC; termB; termA], remaining) // return terms in reverse order so they can easily be appeded to JPTerm list
                                
                            | _ -> Error (sprintf "Too many slashes in axes: %s" input)

                        | None ->
                            Error  (sprintf "Error: Unrecognised axis: %s" axisStr )
            


                // Ok (None, input)  //no error but we don't have an axis
            | Some axisStr -> 
                let axis = getAxis(axisStr)
                match axis with
                | Some axis -> 
                    let termA = (JPTerm.Axis axis)
                    Ok (Some [termA], remaining)
                | None -> Error  (sprintf "Error: Unrecognised axis: %s" axisStr )
    

    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
                
    let parseAndHandleNodeSelection(input: string) : Result<option<list<JPTerm>>* string, string> =
        let (res, remaining) =
            match parseElementSelection(input) with
            | ParseOK (None, _s) -> 
                match parseAttributeSelection(input) with
                | ParseError msg -> ((Error msg), input )
                | ParseOK (None, _s) -> (Ok None, input)
                | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Attribute)),s)
            | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Element) ), s)

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (nodeName, nodeType) ->                                
                Ok (Some [JPTerm.NodeSelection {Name = nodeName; Type = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    let parseAndHandleFunctionName(input: string) : Result<option<list<JPTerm>>* string, string> =
        let (res, remaining) =
            match parseFunctionName(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> 
                Ok (Some x), s

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (functionName) ->
                Ok (Some [JPTerm.NodeName (NodeName.NodeName functionName)], remaining)
                // here we need to process the contents of the function and wrap that in a JPTerm ... or something
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

        
    let parseAndHandleFunctionContents(input: string) : Result<option<list<JPTerm>>* string, string> =
        let (res, remaining) =
            match parseFunctionContents(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> 
                Ok (Some x), s

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (functionName) ->
                Ok (Some [JPTerm.NodeName (NodeName.NodeName functionName)], remaining)
                // here we need to process the contents of the function and wrap that in a JPTerm ... or something
            | None -> Ok (None, remaining)
        | Error msg -> Error msg


    let parseAndHandleXSLFunction(input: string) : Result<option<list<JPTerm>>* string, string> =
        let (res, remaining) =
            match parseElementSelection(input) with
            | ParseOK (None, _s) -> 
                match parseAttributeSelection(input) with
                | ParseError msg -> ((Error msg), input )
                | ParseOK (None, _s) -> (Ok None, input)
                | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Attribute)),s)
            | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Element) ), s)

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (nodeName, nodeType) ->                                
                Ok (Some [JPTerm.NodeSelection {Name = nodeName; Type = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg


    let parseAndHandleSelection(input: string) : Result<option<list<JPTerm>>* string, string> =
        
        let (res, remaining) =
            match parseElementSelection(input) with
            | ParseOK (None, _s) -> 
                match parseAttributeSelection(input) with
                | ParseError msg -> ((Error msg), input )
                | ParseOK (None, _s) -> (Ok None, input)
                | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Attribute)),s)
            | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Element) ), s)

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (nodeName, nodeType) ->                                
                Ok (Some [JPTerm.NodeSelection {Name = nodeName; Type = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg


    let parseAndHandleWhere(input: string) : Result<option<list<JPTerm>>* string, string> =
        
        let (res, remaining) =
            match parseWhere(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> 
                // at this point we have to gather terms for str and wrap that up in a JPTerm.Filter
                // for the moment return a nodename set to this string to parse
                Ok (Some x), s

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (whereClause) ->                                
                Ok (Some [JPTerm.NodeName (NodeName.NodeName whereClause)], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    

    let parseAndHandleElementSelection(input: string) : Result<option<list<JPTerm>>* string, string> =
        
        let (res, remaining) =
            match parseElementSelection(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Element) ), s)

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (nodeName, nodeType) ->                                
                Ok (Some [JPTerm.NodeSelection {Name = nodeName; Type = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    

    let parseAndHandleAttributeSelection(input: string) : Result<option<list<JPTerm>>* string, string> =
        
        let (res, remaining) =
            match parseAttributeSelection(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> (Ok (Some(x, NodeType.Attribute) ), s)

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (nodeName, nodeType) ->                                
                Ok (Some [JPTerm.NodeSelection {Name = nodeName; Type = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    
//---------
//  type ConstantValue = {
//         DataType:DataType;
//         Value: string;
//     }

//     type Filter = {
//         LHS: FilterExpression;
//         RHS: FilterExpression;
//         ComparisonOp: ComparisonOp;
//     } 

//     and FilterExpression = 
//     | Filter of Filter
//     | Constant of ConstantValue


    let parseAndHandleString(quot: char)(input: string) =
        match parseString(quot)(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = ({DataType = DataType.String; Value = str}) |> (Constant >> JPTerm.FilterExpression)
                
                Ok (Some [term], remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    let parseAndHandleNumber(input: string) =
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let dt =
                    match str.Contains "." with
                    | true -> DataType.Float
                    | false -> DataType.Integer

                let term = ({DataType = dt; Value = str}) |> (Constant >> JPTerm.FilterExpression)
                Ok (Some [term], remaining)
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

    let parseAndHandleConstant = (parseAndHandleString singleQuote) >=> parseAndHandleNumber
    
    let rec parseExpression(expr: string) =
        // let rootOp = makeRootOp()
        // let opStack: Stack<BinaryOp> = Stack []
        // let rootStack = Stack.push rootOp opStacks

        let parseResult = parseAndHandleAxes(expr)
        // let parseResult = parseAndHandleElementSelection(expr)
        // let parseResult = parseAndHandleAttributeSelection(expr)
        // let parseResult = parseAndHandleWhere(expr)
        // let parseResult = parseAndHandleFunctionName(expr)
        // let parseResult = parseAndHandleFunctionContents(expr)
        printfn "%A" parseResult

        let rec gatherTerms(input: string, values: list<Value * DataType>, binOps: list<BinaryOp>, expecting: Expecting) : Result<list<Value * DataType> * list<BinaryOp>, string> =    
            // this function does an initial spass through the expression, creating a list of terms
            Error "tbd"
        ()
    
    let testParseExpression(expr:string) = 
        let exprRes = parseExpression(expr) // these values are just placeholders though they are returned from processCalcTree
        printfn "%A" exprRes

    testParseExpression("//element")