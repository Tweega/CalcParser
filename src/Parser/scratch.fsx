
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

     type NodeSelection = {
        NodeName: NodeName;
        NodeType: NodeType;
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
    | Function of Function
    
    and Function =
    | Function of string * FilterExpression * FilterExpression

    and FilterExpression = 
    | Expression of Expression //nodeList
    | Constant of ConstantValue
   
    and Expression =  //expression  evaluates to a node list
        | NodeListExpression of Axis * list<Filter> * JutzPath * NodeSelection //Axis, Filters, Continuation, NodeSelection;

    and JutzPath =
    | Expression
    | Empty

    
    [<RequireQualifiedAccessAttribute>]
    type TermType =
    | Axis
    | NodeSelection


    [<RequireQualifiedAccessAttribute>]
    type TermKey =
    | Axis of Axis
    | TermType of TermType

    type INode =
        abstract getElementNodes : unit -> list<INode>
        abstract getAttributeNodes : unit -> list<INode>
        // abstract getAttributes : unit -> list<IAttribute>
        abstract getAttribute : string -> option<IAttribute>
        // abstract getNodes : NodeType -> list<INode>
        abstract getParent : unit -> INode
        abstract getName : unit -> string
        abstract getNodeType: unit -> NodeType
        // abstract getLinkedNodes: unit -> list<INode>
        // abstract getCategories: unit -> list<string>
        
    and IAttribute =
        inherit INode
        abstract getValue: unit -> string
        abstract getDataType: unit -> DataType
        
    and IAttributeList =
        abstract map : ('T -> 'U) * 'T -> list<'U>
        // abstract bind : ('T -> list<'U>) * 'T -> list<'U>

    // we could add starting position in the original parse string to indicate where failure is tk
    [<RequireQualifiedAccessAttribute>]
    type JPTerm =
    | Axis of Axis
    | FilterExpression of FilterExpression
    | NodeSelection of NodeSelection
    | NodeName of NodeName  //temporary
    | Filter of Filter
    | Function of Function
    | BinaryComparison of BinaryComparison
    | Selector of (list<INode> -> list<INode>)
    with
        member this.getMainType() : TermType =
            match this with
            | JPTerm.Axis _ -> TermType.Axis
            | _ -> TermType.NodeSelection
        member this.getTermKey() =
            match this with
            | JPTerm.Axis axis -> 
                TermKey.Axis axis
            | _ -> TermKey.TermType TermType.NodeSelection
        member this.getInputs() : list<TermType> =
            match this with
            | JPTerm.Axis _ -> [TermType.NodeSelection]
            | JPTerm.NodeSelection _ -> []
            | _ -> []
        

    type TerminationReason =
    | Failure of string
    | Success of list<JPTerm>

    [<RequireQualifiedAccessAttribute>]
    type ParseStatus =
    | Parsing of list<JPTerm>
    | Terminated of TerminationReason //add terminated reason Normal|Error

    type Parserx  = {
        ParseStatus: ParseStatus;
        Run: string -> Result<string * list<JPTerm>, string>;
        Teardown: unit -> unit;
        Terms: list<list<JPTerm>>; //each parse operation returns a list of JPTerms; the list is flattened when parsing complete
        XPath: string;
    }
    with 
        member this.getTerms() = (this.ParseStatus, this.Terms)

    [<RequireQualifiedAccessAttribute>]
    type Request = 
        | Next
        | Stop
    
    [<RequireQualifiedAccessAttribute>]
    type ParseRequest =
        | ParseNext of (Request -> RequestResponse)
        | CancelParse
        
    and RequestResponse = (ParseStatus * ParseRequest) 




    // some axes identify nodes already such as parent and . so we don't need a NodeSelection
    // descendants/* -> the /* is redundant but functions as an element filter
    let xPath = "./station/pump1/@pressure"

    let doubleQuote = '\"'  
    let singleQuote =  '\''   
    
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
 
    let composeXPathsIntersect(f1: list<INode> -> list<INode>)(f2: list<INode> -> list<INode>) =
        fun(iNodes: list<INode>) ->
            iNodes |> (f1 >> f2)

    let (>>=) = composeXPathsIntersect

    // let composeXPathsUnion(f1: list<INode> -> list<INode>, f2: list<INode> -> list<INode>) =
    //     // to implement or we will need getFullPath for each iNode and  this full path will be the key
    //     fun(iNodes1: list<INode>)(iNodes2: list<INode>) ->
    //         let q1 = iNodes1 |> f1
    //         let q2 = iNodes2 |> f2

    let (>=>) = composeParsers

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
                    printfn "remaining::%s" s[m.Groups[1].Value.Length ..]
                    (Ok (Some m.Groups[1].Value), s[m.Length ..])
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
        printfn "String reg ex: %s" reString
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
                ParseOK (maybeFuncName, remaining[.. remaining.Length -  1]) 
                    
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
                ParseOK (maybeWhere, remaining[.. remaining.Length -  1]) //strip off the brackets from the where clause
                    
        | Error msg -> ParseError msg

    let parseAxisChild(s:string) =
        let reChild = "^(\/)[A-Za-z]"
        let newValueResult, remaining = reApplyX(reChild, s)
        match newValueResult with 
        | Ok maybeAxis -> 
        
            match maybeAxis with 
            | None -> ParseOK (None, s)

            | Some _sstr ->
                ParseOK (Some "", s[1..])
                    
        | Error msg -> ParseError msg
    

    let parseAxisDot(s:string) =
        //working here -- this needs an overhaul to account for the fact that axes start with /
        // after all slashes comes an  axis which if missing is child
        let reAxis: string = sprintf @"^\/(\/|\.\.|\.)"  //includes leading and trailing slashes in group match
        // /  starts axis or it ends it?
        //if slash always starts an axis and if the axis nanme does not have :: then shor form assumed
        // so expression always starts /  if ./ we make that /./ so we have /. and / self any child ./parent::element 
        // ./element == self::*/element  and ../ == parent::*/element
        //if fist character of xpath is not slash then prefix one
        // /./element  after a slash comes an axis  which if blank is child /.  /.. //
        // /element
        // to match  
            // (./) self any -- the slash should be left in  remaining .[where]  it would normally be [./@x = 'y']
            // (../) parent any  ../..[where] can you do this? i think so he slash here does not mean child only another axis which if empty ischild
            // (/) 
            // (//)
        
        let newValueResult, remaining = reApplyX(reAxis, s)
        match newValueResult with 
        | Ok maybeAxis -> 
        
            match maybeAxis with 
            | None -> ParseOK (None, s)

            | Some str ->
                ParseOK (maybeAxis, remaining)
                    
        | Error msg -> ParseError msg

    let parseAxisShort (s:string) =
        let ss = parseAxisChild(s)
        match ss with
        | ParseOK (None, _remaining) ->
            parseAxisDot(s)
        | _ -> ss

    
    let parseAnyString (s:string) =
        let ss = parseDoubleQuotedString(s)
        match ss with
        | ParseOK (None, _remaining) ->
            parseQuotedString(s)
        | _ -> ss
    
    let parseConstant (s:string) =
        let ss = parseAnyString(s)
        match ss with
        | ParseOK (None, _remaining) ->
            parseNumber(s)
        | _ -> ss


    let parseAxisLong(s:string) =
        let reAxis: string = sprintf @"^\/([A-Za-z0-9]+)::"  
        
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
        | "" -> Some Axis.Child
        | "child" -> Some Axis.Child
        | "descendant" -> Some Axis.Descendant
        | "ancestor" -> Some Axis.Ancestor
        | "parent" -> Some Axis.Parent
        | "descendant-or-self" -> Some Axis.DescendantOrSelf
        | "ancestor-or-self" -> Some Axis.AncestorOrSelf
        | @"/" -> Some Axis.Descendant
        | _ -> None


    let parseAndHandleAxes(input: string) : Result<option<list<JPTerm>> * string, string> =
        match parseAxisLong(input) with 
        | ParseError msg ->
            Error msg

        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | None -> 
                //long did not work, try short
                // printfn "Trying short"
                match parseAxisShort(input) with 
                | ParseError msg ->
                    Error msg

                | ParseOK (maybeMatch, remaining') -> 
                    match maybeMatch with 
                    | None -> 
                        Ok (None, input)  //no error but we don't have an axis
                    | Some strAxis -> 
                        // Ok (Some str, remaining')
                        printfn "Axis string is %s" strAxis
                        let axis = getAxis(strAxis)
                        match axis with
                        | Some axis' -> 
                            let termAxis = (JPTerm.Axis axis')
                            match axis' with 
                            // self and parent get node selectors of any
                            | Axis.Self | Axis.Parent ->
                                let termAny = (JPTerm.NodeName NodeName.Any)
                                Ok (Some [termAny; termAxis], remaining') // return terms in reverse order so they can easily be appeded to JPTerm list

                            | _ -> 
                                Ok (Some [termAxis], remaining')

                        | None ->
                            Error  (sprintf "Error: Unrecognised axis: %s" strAxis )
    

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
                Ok (Some [JPTerm.NodeSelection {NodeName = (NodeName.NodeName nodeName); NodeType = nodeType}], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    let parseAndHandleFunctionName(input: string) : Result<option<list<JPTerm>>* string, string> =
        let (res, remaining) =
            match parseFunctionName(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> 
                Ok (Some x), "(" + s // replace the opening bracket which was part of the regex wider match

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
                Ok (Some [JPTerm.NodeSelection {NodeName = (NodeName.NodeName nodeName); NodeType = nodeType}], remaining)
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
                Ok (Some [JPTerm.NodeSelection {NodeName = (NodeName.NodeName nodeName); NodeType = nodeType}], remaining)
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
                Ok (Some [JPTerm.NodeSelection {NodeName = (NodeName.NodeName nodeName); NodeType = nodeType}], remaining)
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
                Ok (Some [JPTerm.NodeSelection {NodeName = (NodeName.NodeName nodeName); NodeType = nodeType}], remaining)
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

    //is this function needed - does not do very much working here do we need to sketch out what we are doing??
    // this is different to reading a file
    // there is a point  at which work is done, but the context of a file  read does not change
    // whereas the context of a parsing operation does change
    // we need to carry the JP list through and always have access to the most recent conttext
    // after where clauses we can revert context to an earlier state so context  is the head of the JP  list at the
    //point of entry to the parse function
    let run(pr: ParseRequest) =
        match pr with 
        | ParseRequest.ParseNext reqFun ->
            reqFun Request.Next // ### this is where the recursive function is actually called  and it is the last line

        | ParseRequest.CancelParse ->
            printfn "Parsing already terminated"
            let status = ParseStatus.Terminated (TerminationReason.Failure "Termination request already received")

            (status, ParseRequest.CancelParse)

    let parseInt16 (s:string) : option<int16> = 
        match System.Int16.TryParse(s) with 
        | true, n -> Some n
        | _ -> None

    let parseInt32 (s:string) : option<int32> = 
        match System.Int32.TryParse(s) with 
        | true, n -> Some n
        | _ -> None

    let parseInt64 (s:string) : option<int64> = 
        match System.Int64.TryParse(s) with 
        | true, n -> Some n
        | _ -> None

    let parseFloat32 (s:string) : option<float32> = 
        match System.Single.TryParse(s) with 
        | true, n -> Some n
        | _ -> None
    
    
    let parseFloat64 (s:string) : option<float> = 
        match System.Double.TryParse(s) with 
        | true, n -> Some n
        | _ -> None
   
   
    
    let inline eq<'T when 'T: equality>(t1:'T, t2:'T) = 
        t1 = t2
    let inline gt<'T when ^T: comparison>(t1:'T, t2:'T) = 
        t1 > t2
    let inline lt<'T when ^T: comparison>(t1:'T, t2:'T) = 
        t1 < t2
    let inline gte<'T when ^T: comparison>(t1:'T, t2:'T) = 
        t1 >= t2
    let inline lte<'T when ^T: comparison>(t1:'T, t2:'T) = 
        t1 <= t2

    let  eqInt(iNode:IAttribute, i:int) =
        match parseInt32(iNode.getValue()) with
        | Some i' -> eq(i', i)
        | _ -> false

    let  eqFloat(iNode:IAttribute, f:float) =
        match parseFloat64(iNode.getValue()) with
        | Some f' -> eq(f', f)
        | _ -> false
    
    let  eqString(iNode:IAttribute, s:string) =
        eq(iNode.getValue(), s)

    let filterByName(nodeName:NodeName) (iNodes:list<INode>) =
        iNodes |>
        List.filter(fun i' ->
                match nodeName with 
                | NodeName.Any -> true
                | NodeName.NodeName name -> i'.getName() = name
            )

    type ElementNode = 
        {
            Name: string;
            AttributeMap: Map<string, IAttribute>
            Elements: list<INode>
            Parent:INode;

        }  
        interface INode with
            member this.getName() = this.Name
            member this.getElementNodes() = this.Elements
            member this.getAttributeNodes() = this.AttributeMap |> Map.toList |> List.map(fun i -> snd(i) :> INode)
            member this.getParent() = this.Parent
            member this.getNodeType() = NodeType.Element
            member this.getAttribute(attrName) = Map.tryFind attrName this.AttributeMap

    type AttributeNode = 
        {
            Name: string;
            Value: string;
            Parent: INode;
            AttributeMap: Map<string, IAttribute>
            DataType:DataType;
        }  
        interface IAttribute with
            member this.getName() = this.Name
            member this.getElementNodes() = []
            member this.getAttributeNodes() = this.AttributeMap |> Map.toList |> List.map(fun i -> snd(i) :> INode)
            member this.getParent() = this.Parent
            member this.getNodeType() = NodeType.Attribute
            member this.getValue() = this.Value
            member this.getDataType() = this.DataType;
            member this.getAttribute(attrName) = Map.tryFind attrName this.AttributeMap
            
            // interface IAttribute


    let getNodes({NodeName = nodeName; NodeType = nodeType}: NodeSelection)(nodes: list<INode>) : list<INode> =
        let fetchNodes = 
            match nodeType with
            | NodeType.Attribute -> 
                fun(iNode:INode) -> 
                    iNode.getAttributeNodes()
            | NodeType.Element ->
                fun(iNode:INode) -> 
                    iNode.getElementNodes()

        nodes |>
        List.fold(fun acc iNode ->
            let filteredNodes = fetchNodes(iNode) |> filterByName nodeName
            filteredNodes |>
            List.fold(fun acc' i' ->
                i' :: acc'
            ) acc
        ) []

    let getSelf(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        match nodeName with 
        | NodeName.Any -> nodes
        | NodeName.NodeName nodeName ->
            nodes |>
            List.filter(fun i ->
                i.getName() = nodeName
            )
            
    let getDescendantsOrSelf(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        // this will mangle document order
        // if we need to preserve document order we need to index elements in order that they were read in
        // their internal order should be oreserved by the folds, but subsequent sorting might lose that
        // wemay eventually want to index against parent nodes or similar. Ignore order for now
        let rec descendants(nodesToProcess:list<INode>, acc) =
            match nodesToProcess with
            | [] -> acc
            | h :: t ->
                let newAcc = 
                    [h]
                    |> (filterByName nodeName)
                    |> List.fold (fun acc' i ->
                        i :: acc'
                    ) acc
                
                // add child elements to list of nodes to be processed, ignoring head which is being assessed now.
                // this doesnot inclde attributes.  to  get those add an axis as in .//pump/@pressure
                match h.getElementNodes() with 
                | []  -> 
                    newAcc                    
                | childElements ->                    
                    let newProcessList = 
                        childElements |> 
                        List.fold (fun acc' i ->
                            i :: acc'
                        ) t  
                    descendants (newProcessList, newAcc)
        descendants(nodes, [])
    
    let getDescendants(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        let childNodes = getNodes({NodeName=NodeName.Any; NodeType=NodeType.Element})(nodes)
        getDescendantsOrSelf nodeName childNodes

    
    let getParent(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        nodes |>
        List.fold(fun acc iNode ->
            let filteredNodes = [iNode.getParent()] |> filterByName nodeName
            filteredNodes |>
            List.fold(fun acc' i' ->
                i' :: acc'
            ) acc
        ) []

        
        
    let getAncestorsOrSelf(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        // this also mangles document order review tk - possibly we add child then all its children
        // so perhaps we only need to reverse the list before returning - this would be for a depth first order
        let rec ancestors(nodesToProcess:list<INode>, acc) =
            match nodesToProcess with
            | [] -> acc
            | h :: t ->
                let newAcc = 
                    [h]
                    |> (filterByName nodeName)
                    |> List.fold (fun acc' i ->
                        i :: acc'
                    ) acc
                
                // add parent elements to list of nodes to be processed, ignoring head which is being assessed now.
                let newProcessList = 
                    h.getParent() :: t  
                ancestors (newProcessList, newAcc)
        ancestors(nodes, [])
        
    let getAncestors(nodeName: NodeName)(nodes: list<INode>) : list<INode> =
        let parentNodes = getParent nodeName nodes
        getAncestorsOrSelf nodeName parentNodes

    // is this a lift? applying avalue to a function is a way of extracting data so we lift the second function into the first?    
    let inline lift<'a, 'b, 'c> (bc: 'b ->'c) (ab: 'a -> 'b) : 'a -> 'c =
        fun (a:'a) ->
            a |> (ab >> bc)

    // let xp = "./pump"
    // let xp' = 
    //     fun (str:string) (nodes) ->
    //         getElements (NodeName.NodeName str) nodes

    
    // let xp2 = "./pump/pressure"
    // let xp2' = 
    //     fun (str:string) (nodes) ->
    //         getElements (NodeName.NodeName str) nodes
    
    // let f1 = getElements (NodeName.NodeName "pump")
    // let f2 = getAttributes (NodeName.NodeName "pressure")
    // let f3 = f1 >>= f2 //alernatively lift f1 f2

    // [axis:child][nodename:pump][where:JPList]function:name, JPList,JP:ist]
    //so we can have functions that expect differing argument numbers
    //better to read in as xml and apply xpath? maybe
    // a function places on the stack a cheeseboard. when this is full it is added to accumulator of NodeList->NodeList
    // which will be composed together

        
    type ArgError = option<string>
    type XPValue = {
        JPTerm: JPTerm;
    }

    type FunctionBuilder = 
        {
            JPTerm: JPTerm;
            InputTypes: list<TermType>;
            Inputs: list<JPTerm>;
            Arity: int;
            MaybeError: option<string>;
            MakeFunction: list<JPTerm> -> (list<INode> -> list<INode>)
        } with
        
        member this.getArity() = this.Arity;
        member this.applyArg(term:JPTerm) =
            match this.MaybeError with
            | None ->
                match this.InputTypes with
                | [] -> 
                    let msg = sprintf "No values expected when adding term"
                    Error msg
                | inType :: t -> 
                    
                    let termType = term.getMainType()
                    match termType with
                        | Eq inType ->
                            Ok ({this with Inputs = term :: this.Inputs; InputTypes = t}, t.Length)
                        | _ ->
                            let msg = sprintf "Type mismatch: %A does not match %A" inType termType
                            Error msg
            | Some err -> Error err

        member this.getInputs() =
            this.Inputs |> List.rev

        
        member this.makeFunction() =
            this.Inputs |> this.MakeFunction


    [<RequireQualifiedAccessAttribute>]
    type XPTerm = 
    | Value of JPTerm
    | Builder of FunctionBuilder

    type XPathStack = list<XPTerm>
    type NodeTansformStack = list<list<INode> -> list<INode>>
    
    // a wrapper  function so that all axis functions get passed NodeSelection though not  all axes need node type
    let ff(g:NodeName -> list<INode> -> list<INode>) =
        // for functions that only select Elements and do not need to distinguish on node type
        fun (nodeSelection:NodeSelection) ->
            g nodeSelection.NodeName
                        
    
    let axisMapList =
        [
            (Axis.Self, (ff getSelf))
            (Axis.Child, getNodes) //this function needs to know  the node type
            (Axis.Parent, ff getParent)
            (Axis.Ancestor, ff getAncestors)
            (Axis.AncestorOrSelf, ff getAncestors)
            (Axis.Descendant, ff getDescendants)
            (Axis.DescendantOrSelf, ff getDescendants)
        ]

    let noOp = fun iNodes -> iNodes
    //This defines the arguments a function is expecting -for Axis child, just a node selection
    let childAxisBuilder: FunctionBuilder = {
        JPTerm = JPTerm.Axis Axis.Child;
        InputTypes = [TermType.NodeSelection];
        Inputs = [];
        Arity = 1;
        MaybeError = None;
        MakeFunction = fun(inputs) ->
            // inputs will already have been validated via applyArg
            match inputs with
            | jpTerm :: [] ->
                match jpTerm with 
                | JPTerm.NodeSelection nodeSelection ->
                    let f = getNodes nodeSelection
                    fun iNodes ->
                        f iNodes
                | otherType -> 
                    let msg  = sprintf "Arg error. Expected NodeSelection,  got %A" otherType
                    printfn "%s" msg
                    noOp
            | _ -> 
                let msg = sprintf "Should not see this.  Error: expected 1 inmput, got %d" inputs.Length
                printfn "%s" msg
                noOp        
    }

    let builderMap:Map<TermKey, XPTerm> = 
        [
            (TermKey.Axis Axis.Child, XPTerm.Builder childAxisBuilder)
            (TermKey.TermType TermType.NodeSelection, XPTerm.Builder childAxisBuilder)
        ] 
        |> Map.ofList



    let jpTerms' = [JPTerm.Axis Axis.Child; JPTerm.NodeSelection {NodeName = NodeName.NodeName "pump"; NodeType = NodeType.Element};]
    let xpTerms = 
        let (terms, status') =
            jpTerms' |>
            List.fold(fun  (acc: list<XPTerm>, status:option<string>) i ->
                match status with 
                | Some _err -> (acc, status)
                | None -> 
                    let  key = i.getTermKey()
                    match builderMap with
                    | Exists key xpTerm ->
                        xpTerm :: acc, None
                    | _ -> 
                        let msg = sprintf "No XPTerm found in builder map for %A" key
                        printfn "%s" msg
                        (acc, Some msg)
            )([],None)
        match status' with 
        | Some err ->
            printfn "Error: %s" err
            []
        | None -> terms


    let compileXPathFromTerms(xpTerms:list<XPTerm>) = 
        let rec applyTerm(accSelectors':NodeTansformStack, accBuilders':list<FunctionBuilder>, jpTerm': JPTerm) =
            printfn "Applyin JPTerm: %A" jpTerm'
            
            match accBuilders' with
            | h :: t ->
                let applicationResult = h.applyArg(jpTerm')
                match applicationResult with
                | Ok (builder: FunctionBuilder, arity:int) ->
                    match arity with
                    | Eq 0 -> 
                        let selector = builder.makeFunction()
                        applyTerm(selector :: accSelectors', t, JPTerm.Selector selector)
                    | _ ->
                        // this builder still needs inputs
                        Ok (accSelectors', builder :: t)
                | Error err ->
                    // (accSelectors', accBuilders')
                    Error err
                    
            | [] -> 
                // no builder, see if we have a selector
                match jpTerm' with
                | JPTerm.Selector selector ->
                    Ok (selector :: accSelectors', accBuilders')
                | _ -> 
                    let msg = sprintf "Top level term is not a selector: %A" jpTerm'
                    Error msg

        printfn "xpTerms: %A" xpTerms

        let (status, transformStack, builders) =
            xpTerms |>
            List.fold(fun (maybeError:option<string>, accSelectors:NodeTansformStack, accBuilders:list<FunctionBuilder>) xpTerm ->
                match maybeError with 
                | Some _err -> maybeError, accSelectors, accBuilders
                | None ->
                    match xpTerm with
                    | XPTerm.Builder builder ->
                        None, accSelectors, builder :: accBuilders
                    | XPTerm.Value jpTerm ->
                        match applyTerm (accSelectors, accBuilders, jpTerm) with
                        | Ok (nodeTransformStack, builderStack) ->
                            (None, nodeTransformStack, builderStack)
                        | Error err ->
                            (Some err, accSelectors, accBuilders)

                // acc.applyArg(i)
            )(None, [],[])
        
        match status with 
        | Some err -> Error err
        |None -> 
            match builders.Length with 
            | Eq 0 ->
                Ok transformStack
            | _ -> Error "builders left over"


    let gg = compileXPathFromTerms(xpTerms)
        
    // let inputs = argApplicationResult'.getInputs()

    // let parseConstant = (parseAndHandleString) >=> parseAndHandleNumber
    
    let rec parseExpression(expr: string) =
        // let rootOp = makeRootOp()
        // let opStack: Stack<BinaryOp> = Stack []
        // let rootStack = Stack.push rootOp opStacks

        // let parseResult = parseAndHandleAxes(expr)
        // let parseResult = parseAndHandleElementSelection(expr)
        // let parseResult = parseAndHandleAttrib/zuteSelection(expr)
        let parseResult = parseAndHandleWhere(expr)
        // let parseResult = parseAndHandleFunctionName(expr)
        let parseResult = parseAndHandleFunctionContents(expr)
        // let parseResult = parseAndHandleConstant(expr)
        printfn "%A" parseResult

        let rec gatherTerms(input: string, values: list<Value * DataType>, binOps: list<BinaryOp>, expecting: Expecting) : Result<list<Value * DataType> * list<BinaryOp>, string> =    
            // this function does an initial pass through the expression, creating a list of terms
            Error "tbd"
        ()
    
    let testParseExpression(expr:string) = 
        let exprRes = parseExpression(expr) // these values are just placeholders though they are returned from processCalcTree
        printfn "%A" exprRes

    parseConstant("'hello' there")
