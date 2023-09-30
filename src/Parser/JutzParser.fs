﻿namespace Parser

module JutzParser =
    open ParserTypes
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

    let constant c _ = c

    
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

    [<RequireQualifiedAccessAttribute>]
     type NodeSelection = {
        NodeName: NodeName;
        NodeType: NodeType;
    }

    type ConstantValue = {
        DataType:DataType;
        Value: string;
    }

    
    type INode =
        abstract getElementNodes : unit -> list<INode>
        abstract getAttributeNodes : unit -> list<INode>
        // abstract getAttributes : unit -> list<IAttribute>
        abstract getAttribute : string -> option<IAttribute>
        // abstract getNodes : NodeType -> list<INode>
        abstract getParent : unit -> option<INode>
        abstract getName : unit -> string
        abstract getNodeType: unit -> NodeType
        // abstract getLinkedNodes: unit -> list<INode>
        // abstract getCategories: unit -> list<string>
        abstract getValue: unit -> string
        
    and IAttribute =
        inherit INode
        abstract getDataType: unit -> DataType
        
    type IAttributeList =
        abstract map : ('T -> 'U) * 'T -> list<'U>
        // abstract bind : ('T -> list<'U>) * 'T -> list<'U>

    // we could add starting position in the original parse string to indicate where failure is tk
    // JutzPath terms reflect the basicunitsof parsing from XPath strings
    //              
    //         >=>  
    //         >=>  
    //         >=>  
    //         >=>  
    //         >=> tryParseFunctionContents 
    //         >=> 

    type Selector = list<INode> -> list<INode>

    [<RequireQualifiedAccessAttribute>]
    type JPTerm =
    | Axis of Axis  //tryParseAxes
    // | FilterExpression of FilterExpression  
    | NodeSelection of NodeSelection //tryParseElementSelection, tryParseAttributeSelection
    // | NodeName of NodeName  //temporary
    // | Filter of Filter  //tryParseWhere
    | FunctionName of FunctionName //tryParseFunctionName
    // | BinaryComparison of BinaryComparison
    | ComparisonOp of ComparisonOp // tryParseBinaryOperator
    | ConstantValue of ConstantValue //tryParseConstant
    | OpenFilter
    | CloseFilter

    // synthesised during building so perhaps an XPTerm
    | Selector of Selector
    with
        member this.getTermType() : TermType =
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
            
    and  [<RequireQualifiedAccessAttribute>]
        FilterExpression = 
        | Expression of list<JPTerm> //nodeList
        | ConstantValue of ConstantValue

    
    and [<RequireQualifiedAccessAttribute>]
        Filter = 
        | BinaryComparison of BinaryComparison
        | Function of Function
    
    
    and [<RequireQualifiedAccessAttribute>]
        BinaryComparison = {
            LHS: FilterExpression;
            RHS: FilterExpression;
            ComparisonOp: ComparisonOp;
        } 

    and [<RequireQualifiedAccessAttribute>]
        Function =
        | Function of string * FilterExpression * FilterExpression  //complex objects are XPTerms not JPTerms

    //how do these tie in to the underlying types? tk  
    // these are te types of the basic parse units
    and [<RequireQualifiedAccessAttribute>]
        TermType =
        | Axis
        | NodeSelection
        | NodeName
        | Selector
        | FunctionName
        | ComparisonOp
        | ConstantValue
        | OpenFilter
        | CloseFilter


    and  [<RequireQualifiedAccessAttribute>]
        TermKey =
        | Axis of Axis
        | TermType of TermType

    and [<RequireQualifiedAccessAttribute>]
        XPTerm = 
        | XPValue of JPTerm
        | Builder of FunctionBuilder

    and FunctionBuilder = 
        {
            // TermType: TermType;  //for labelling purposes only
            InputTypes: list<TermType>;
            Inputs: list<JPTerm>;
            // Arity: int;
            MaybeError: option<string>;
            ApplyArg: (FunctionBuilder * JPTerm) -> ApplyArgResult
            MakeFunction: list<JPTerm> -> (list<INode> -> list<INode>)
        } with

        static member applyArgDefault(fb: FunctionBuilder, jpTerm: JPTerm) =
            match fb.MaybeError with
            | None ->
                match fb.InputTypes with
                | [] -> 
                    let msg = sprintf "JPTerm submitted forapplication,butt no term expected %A" jpTerm
                    ApplyArgResult.Error msg
                | inType :: t -> 
                    
                    let termType = jpTerm.getTermType()
                    match termType with
                        | Eq inType ->
                            match t.Length with
                            | Eq 0 ->
                                ApplyArgResult.Complete {fb with Inputs = jpTerm :: fb.Inputs; InputTypes = t}
                            | _ ->
                                ApplyArgResult.Incomplete {fb with Inputs = jpTerm :: fb.Inputs; InputTypes = t}
                        | _ ->
                            let msg = sprintf "Type mismatch: %A does not match %A" inType termType
                            ApplyArgResult.Error msg
            | Some err -> ApplyArgResult.Error err

        
        member this.applyArg(term:JPTerm) =
            this.ApplyArg(this, term)
            
        member this.getInputs() =
            this.Inputs |> List.rev
        
        member this.makeFunction() =
            this.Inputs |> this.MakeFunction

    and [<RequireQualifiedAccessAttribute>]
        ApplyArgResult =
        | Complete of FunctionBuilder
        | Incomplete of FunctionBuilder
        | Error of string  //include the builder along with the error message? tk

    type SelectorStack = list<list<INode> -> list<INode>>
    type XPathStack = list<XPTerm>

    [<RequireQualifiedAccessAttribute>]
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

    
    let parseOpenWhere(s:string) =
        let reWhere: string = sprintf @"^(\[)"
        
        let newValueResult, remaining = reApplyX(reWhere, s)
        match newValueResult with 
        | Ok maybeWhere -> 
        
            match maybeWhere with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeWhere, remaining)
                    
        | Error msg -> ParseError msg

    
    let parseCloseWhere(s:string) =
        let reWhere: string = sprintf @"^(\])"
        
        let newValueResult, remaining = reApplyX(reWhere, s)
        match newValueResult with 
        | Ok maybeWhere -> 
        
            match maybeWhere with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeWhere, remaining)
                    
        | Error msg -> ParseError msg

    
    let parseOpenFunction(s:string) =
        let reWhere: string = sprintf @"^(\[)"
        
        let newValueResult, remaining = reApplyX(reWhere, s)
        match newValueResult with 
        | Ok maybeWhere -> 
        
            match maybeWhere with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeWhere, remaining)
                    
        | Error msg -> ParseError msg

    
    let parseCloseFunction(s:string) =
        let reWhere: string = sprintf @"^(\))"
        
        let newValueResult, remaining = reApplyX(reWhere, s)
        match newValueResult with 
        | Ok maybeWhere -> 
        
            match maybeWhere with 
            | None -> ParseOK (None, s)

            | Some _str ->
                ParseOK (maybeWhere, remaining)
                    
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

    let parseComparisonOperator(s:string) =
        let reOp = @"^(>=|>|<=|<|=)"
        let newValueResult, remaining = reApplyX(reOp, s)      
        match newValueResult with 
        | Ok maybeOperator ->
            ParseOK (maybeOperator, remaining)
        | Error msg -> ParseError msg


    let rec anyOf(tests: list<unit -> bool>) =
        match tests with
        | h :: t ->
            match h() with
            |true -> true
            | false -> anyOf(t)
        | [] -> false
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


    let tryParseAxes(input: string) : Result<option<list<JPTerm>> * string, string> =
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
                                let termAny = (JPTerm.NodeSelection {NodeName = NodeName.Any; NodeType = NodeType.Element})
                                Ok (Some [termAxis; termAny], remaining') 

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
    
        
                
    let tryParseNodeSelection(input: string) : Result<option<list<JPTerm>> * string, string> =
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

    let tryParseFunctionName(input: string) : Result<option<list<JPTerm>> * string, string> =
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
                Ok (Some [JPTerm.FunctionName functionName], remaining)
                // here we need to process the contents of the function and wrap that in a JPTerm ... or something
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    let tryParseConstant(input: string) : Result<option<list<JPTerm>> * string, string> =
        let (res, remaining) =
            match parseConstant(input) with
            | ParseOK (None, _s) -> 
                 (Ok None, input)
            | ParseOK (Some x, s) -> 
                Ok (Some x), s 

            | ParseError msg -> (Error msg, input)

        match res with
        | Ok maybeMatch -> 
            match maybeMatch with 
            | Some (c) ->
                Ok (Some [JPTerm.ConstantValue {DataType = DataType.String; Value = c}], remaining)
                // here we need to process the contents of the function and wrap that in a JPTerm ... or something
            | None -> Ok (None, remaining)
        | Error msg -> Error msg

    let tryParseXSLFunction(input: string) : Result<option<list<JPTerm>> * string, string> =
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


    let tryParseSelection(input: string) : Result<option<list<JPTerm>> * string, string> =
        
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


    let tryParseWhereOpen(input: string) : Result<option<list<JPTerm>> * string, string> =
        
        let (res, remaining) =
            match parseOpenWhere(input) with
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
            | Some (_whereClause) ->                                
                Ok (Some [JPTerm.OpenFilter], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg


    let tryParseWhereClose(input: string) : Result<option<list<JPTerm>> * string, string> =
        
        let (res, remaining) =
            match parseCloseWhere(input) with
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
            | Some (_whereClause) ->                                
                Ok (Some [JPTerm.CloseFilter], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg
    


    let tryParseFunctionOpen(input: string) : Result<option<list<JPTerm>> * string, string> =
        
        let (res, remaining) =
            match parseOpenFunction(input) with
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
            | Some (_whereClause) ->                                
                Ok (Some [JPTerm.OpenFilter], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg


    let tryParseFunctionClose(input: string) : Result<option<list<JPTerm>> * string, string> =
        
        let (res, remaining) =
            match parseCloseFunction(input) with
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
            | Some (_whereClause) ->                                
                Ok (Some [JPTerm.CloseFilter], remaining)
            | None -> Ok (None, remaining)
        | Error msg -> Error msg
    

    let tryParseElementSelection(input: string) : Result<option<list<JPTerm>> * string, string> =
        
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

    

    let tryParseAttributeSelection(input: string) : Result<option<list<JPTerm>> * string, string> =
        
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

    let tryParseComparisonOperator(input: string) =
        match parseComparisonOperator(input) with 

        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some opStr ->
                // printfn "OP: %s" opStr
                let maybeOp = 
                    match opStr with 
                    | "=" -> Some ComparisonOp.EQ
                    | ">" -> Some ComparisonOp.GT
                    | ">=" -> Some ComparisonOp.GTE
                    | "<" -> Some ComparisonOp.LT
                    | "<=" -> Some ComparisonOp.LTE
                    | _ -> None

                match maybeOp with 
                | Some binOp -> 
                    Ok (Some [JPTerm.ComparisonOp binOp], remaining)
                | None -> Ok(None, input)


            | None ->
                Ok (None, input)

        | ParseError msg ->
            Error msg

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


    let tryParseString(quot: char)(input: string) =
        match parseString(quot)(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = ({DataType = DataType.String; Value = str}) |> JPTerm.ConstantValue
                
                Ok (Some [term], remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    let tryParseNumber(input: string) =
        match parseNumber(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let dt =
                    match str.Contains "." with
                    | true -> DataType.Float
                    | false -> DataType.Integer

                let term = ({DataType = dt; Value = str}) |> JPTerm.ConstantValue
                Ok (Some [term], remaining)
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg

    
    let tryParseTag(input: string) =
        match parseTag(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Tag >> Value))
                Ok (Some (term, DataType.Numeric), remaining) // assume tags emit numeric values for the moment
            | None -> Ok (None, input)
            
        | ParseError msg ->
            Error msg
    
    
    let tryParsePath(input: string, terms: list<TypedTerm>) =
        match parseTag(input) with 
        | ParseOK (maybeMatch, remaining) -> 
            match maybeMatch with 
            | Some str -> 
                let term = str |> ((Path >> Value))
                Ok (Some (term, DataType.Numeric), remaining) // assume that attributes return numeric values for the moment
            | None -> Ok (None, input)
            
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
   
   
    
    let inline eq<'T when 'T: equality>(t1:'T) (t2:'T) = 
        t1 = t2
    let inline gt<'T when ^T: comparison>(t1:'T) (t2:'T) = 
        t1 > t2
    let inline lt<'T when ^T: comparison>(t1:'T) (t2:'T) = 
        t1 < t2
    let inline gte<'T when ^T: comparison>(t1:'T) (t2:'T) = 
        t1 >= t2
    let inline lte<'T when ^T: comparison>(t1:'T) (t2:'T) = 
        t1 <= t2

    let compareInt(op)(lhs:INode)(rhs:INode) =
        let comparator = 
            match op with
            | ComparisonOp.EQ -> eq
            | ComparisonOp.GT -> gt
            | ComparisonOp.LT -> lt
            | ComparisonOp.GTE -> gte
            | ComparisonOp.LTE -> lte
        
        match (
            parseInt32(lhs.getValue()) 
            |> Option.bind (fun v1 -> 
                parseInt32(rhs.getValue()) 
                |> Option.bind(fun v2 -> 
                    Some(comparator v1 v2)
                )
            )
        ) with
        | Some b -> b
        | None -> false

    let compareFloat(op)(lhs:INode)(rhs:INode) =
        let comparator = 
            match op with
            | ComparisonOp.EQ -> eq
            | ComparisonOp.GT -> gt
            | ComparisonOp.LT -> lt
            | ComparisonOp.GTE -> gte
            | ComparisonOp.LTE -> lte
        
        match (
            parseFloat64(lhs.getValue()) 
            |> Option.bind (fun v1 -> 
                parseFloat64(rhs.getValue()) 
                |> Option.bind(fun v2 -> 
                    Some(comparator v1 v2)
                )
            )
        ) with
        | Some b -> b
        | None -> false

    let compareString(op)(lhs:INode)(rhs:INode) =
        let comparator = 
            match op with
            | ComparisonOp.EQ -> eq
            | ComparisonOp.GT -> gt
            | ComparisonOp.LT -> lt
            | ComparisonOp.GTE -> gte
            | ComparisonOp.LTE -> lte
        
        comparator (lhs.getValue())(rhs.getValue())

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
            Parent:option<INode>;

        }  
        interface INode with
            member this.getName() = this.Name
            member this.getElementNodes() = this.Elements
            member this.getAttributeNodes() = this.AttributeMap |> Map.toList |> List.map(fun i -> snd(i) :> INode)
            member this.getParent() = this.Parent
            member this.getNodeType() = NodeType.Element
            member this.getAttribute(attrName) = Map.tryFind attrName this.AttributeMap
            member this.getValue() = 
                let iNode = this :> INode 
                let attributes =
                    iNode.getAttributeNodes() |>
                    List.map (fun i -> i :?> IAttribute)
                    |> List.fold(fun acc v ->
                        v.getValue() :: acc
                    )[]
                System.String.Join('|', attributes)

    type AttributeNode = 
        {
            Name: string;
            Value: string;
            Parent: option<INode>;
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
            let filteredNodes = 
                match iNode.getParent() with
                | Some parent -> [parent] |> filterByName nodeName
                |None -> []
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
                    match h.getParent() with
                    | Some parent -> parent :: t  
                    | None -> t
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

    let makeAttributeFromConstant(c:ConstantValue) =
        //make an attribute node from constant
        {
            Name = "Constant";
            Value = c.Value;
            Parent = None;
            AttributeMap  =  Map.empty
            DataType= c.DataType;
        }
        
    
    // a wrapper  function so that all axis functions get passed NodeSelection though not  all axes need node type
    let ff(g:NodeName -> list<INode> -> list<INode>) =
        // for functions that only select Elements and do not need to distinguish on node type
        fun (nodeSelection:NodeSelection) ->
            g nodeSelection.NodeName
                        
    //This defines the arguments a function is expecting -for Axis child, just a node selection

    let composeSelectors(s1:Selector) (s2:Selector) =
        lift s1 s2
    
    let makeAxisBuilder(axis:Axis, selector:NodeSelection -> list<INode>-> list<INode>) : FunctionBuilder =
        {
            InputTypes = [TermType.NodeSelection];
            Inputs = [];            
            MaybeError = None;
            ApplyArg = fun(this, jpTerm) -> FunctionBuilder.applyArgDefault(this, jpTerm)
            MakeFunction = fun(inputs) ->
                // inputs will already have been validated via applyArg
                match inputs with
                | jpTerm :: [] ->
                    match jpTerm with 
                    | JPTerm.NodeSelection nodeSelection ->
                        let f = selector nodeSelection
                        fun iNodes ->
                            f iNodes
                    | otherType -> 
                        let msg  = sprintf "Arg error. Expected NodeSelection,  got %A" otherType
                        printfn "%s" msg
                        id
                | _ -> 
                    let msg = sprintf "Should not see this.  Error: expected 1 inmput, got %d" inputs.Length
                    printfn "%s" msg
                    id        
        }

    // adds extra unit parameter to function so that it does not immediately execute when it gets all its other parameters
    // only works as expected when called with a function expecting a single argument.
    let delay(f: 'T-> 'U) =
        fun(t: 'T) -> 
            fun() ->
                f t

    // have mapJPXP provate to compile function meaning that everything else just deals with JPTerms
    let rec compileXPathFromTerms(jpTerms:list<JPTerm>) = 
        let xpTerms = mapJPXP(jpTerms)
        let rec applyTerm(accSelectors':SelectorStack, accBuilders':list<FunctionBuilder>, jpTerm: JPTerm) =
            printfn "Applying JPTerm: %A" jpTerm
            
            match accBuilders' with
            | h :: t ->
                let applyResult = h.ApplyArg(h, jpTerm)
                match applyResult with
                | ApplyArgResult.Incomplete fb ->
                    Ok (accSelectors', fb :: t)
                | ApplyArgResult.Complete fb ->
                    let selector = fb.makeFunction()
                    applyTerm(accSelectors', t, JPTerm.Selector selector)
                | ApplyArgResult.Error err -> Error err
            
                    
            | [] -> 
                // no builder, see if we have a selector
                match jpTerm with
                | JPTerm.Selector selector ->
                    Ok (selector :: accSelectors', accBuilders')
                | _ -> 
                    let msg = sprintf "Top level term is not a selector: %A" jpTerm
                    Error msg

        printfn "xpTerms: %A" xpTerms

        let (status, selectorStack, builders) =
            xpTerms |>
            List.fold(fun (maybeError:option<string>, accSelectors:SelectorStack, accBuilders:list<FunctionBuilder>) xpTerm ->
                match maybeError with 
                | Some _err -> maybeError, accSelectors, accBuilders
                | None ->
                    match xpTerm with
                    | XPTerm.Builder builder ->
                        None, accSelectors, builder :: accBuilders
                    | XPTerm.XPValue jpTerm ->
                        match applyTerm (accSelectors, accBuilders, jpTerm) with
                        | Ok (selectorStack, builderStack) ->
                            (None, selectorStack, builderStack)
                        | Error err ->
                            (Some err, accSelectors, accBuilders)

                // acc.applyArg(i)
            )(None, [],[])
            
        match status with 
        | Some err -> Error err
        |None -> 
            match builders.Length with 
            | Eq 0 ->
                let selector =
                    selectorStack |>
                    List.rev
                    |> List.reduce composeSelectors

                Ok selector
            | _ -> Error "builders left over"

    

    and comparisonFilterBuilder() : FunctionBuilder =
        {
            InputTypes = [TermType.OpenFilter; TermType.Selector; TermType.ComparisonOp; TermType.Selector; TermType.CloseFilter];
            Inputs = [];
            MaybeError = None;
            ApplyArg = fun(this, jpTerm) -> FunctionBuilder.applyArgDefault(this, jpTerm)
            MakeFunction = fun(inputs) ->   //this function is called via FunctionBuilder.makeFunction which passes in inputs list
                // inputs will already have been validated via applyArg
                match inputs with
                | lhsTerm :: binOp :: rhsTerm :: [] ->
                    match (lhsTerm, binOp, rhsTerm) with 
                    | (JPTerm.Selector lhsSelector, JPTerm.ComparisonOp op, JPTerm.Selector rhsSelector) ->
                        
                        fun (iNodes:list<INode>) ->
                            iNodes |>
                            List.filter(fun iNode ->
                                //assuming for now that current node is always the root deal with global later tk
                                let lhsNodes = [iNode] |> lhsSelector
                                let rhsNodes = [iNode] |> rhsSelector

                                let typedComparator = 
                                    match lhsNodes with
                                    | h :: _t ->
                                        let nodeType = h.getNodeType()
                                        match nodeType with
                                        | NodeType.Element ->
                                            compareString
                                        | NodeType.Attribute ->
                                            match tryUnbox<IAttribute> h with
                                            | Some iAttribute ->
                                            
                                                match iAttribute.getDataType() with
                                                | DataType.Float ->
                                                    compareFloat
                                                | DataType.Integer ->
                                                    compareInt
                                                | _ ->
                                                    compareString
                                            
                                            | None -> compareString //default
                                    | [] -> compareString //default

                                let tests =
                                    lhsNodes |>
                                    List.fold(fun acc lhsNode ->
                                        rhsNodes |>
                                        List.fold(fun acc' rhsNode ->
                                            (delay (typedComparator op lhsNode)) rhsNode :: acc'
                                        ) acc
                                    ) []

                                anyOf(tests)
                            )
                    | _ -> 
                        printfn "Error Expecting JPTerm.FilterExpression lhsExpr, JPTerm.ComparisonOp op, JPTerm.FilterExpression rhsExpr but got %A" inputs 
                        fun _nodes -> []
                | _ -> 
                    printfn "Error Expecting 3 inputs, got %d : %A" inputs.Length inputs 
                    printfn "Error" 
                    fun _nodes -> []
        }


    and mapJPXP(jpTerms':list<JPTerm>) = 
        let stringFunctionMap = // string -> string -> bool
            [
                ("contains", fun(s1:string, s2:string) -> s1.Contains(s2));
                ("starts-with", fun(s1:string, s2:string) -> s1.StartsWith(s2));
                ("ends-with", fun(s1:string, s2:string) -> s1.EndsWith(s2));
            ]
        let resolvedConstantMap =
            [
                ("mid", fun(s1:string, startIndex:int, endIndex:int) -> s1[startIndex..endIndex]);

            ]
        let builderMap = 
            [
            (TermKey.Axis Axis.Child, makeAxisBuilder (Axis.Child, getNodes))
            (TermKey.Axis Axis.Parent, makeAxisBuilder (Axis.Parent, ff getParent))
            (TermKey.Axis Axis.Ancestor, makeAxisBuilder (Axis.Ancestor, ff getAncestors))
            (TermKey.Axis Axis.AncestorOrSelf, makeAxisBuilder (Axis.AncestorOrSelf, ff getAncestorsOrSelf))
            (TermKey.Axis Axis.Descendant, makeAxisBuilder (Axis.Descendant, ff getDescendants))
            (TermKey.Axis Axis.DescendantOrSelf, makeAxisBuilder (Axis.DescendantOrSelf, ff getDescendantsOrSelf))
            (TermKey.Axis Axis.Self, makeAxisBuilder (Axis.Self, ff getSelf))
            (TermKey.TermType TermType.OpenFilter, filterDelegator())
            // (TermKey.TermType TermType.NodeSelection, XPTerm.Value )
            ] |> Map.ofList

        let (terms, status') =
            jpTerms' |>
            List.fold(fun  (acc: list<XPTerm>, status:option<string>) jpTerm ->
                match status with 
                | Some _err -> (acc, status)
                | None -> 
                    let  key = jpTerm.getTermKey()
                    match builderMap with
                    | Exists key functionBuilder ->
                        let acc' = (XPTerm.Builder functionBuilder) :: acc
                        acc', None

                    | _ -> 
                        // assume that this JPTerm is a value - find away to check this? tk
                        (XPTerm.XPValue jpTerm) :: acc, None
            )([],None)
        match status' with 
        | Some err ->
            printfn "Error: %s" err
            []
        | None -> terms
        |> List.rev

    // filterBuilder is created when we get an open square bracket  [
    // it collects terms until it can determine a builder to delegate to
    // which in this case will be one of FunctionBuilder or ComparisonBuilder
    // the filter delegator will expect a selector as the first parameter
    // which might either be a function or the beginnings of a comparison
    // the second argument will either be close scope or  an operator
    and filterDelegator() : FunctionBuilder =
        {
            InputTypes = []; //filterBuilder delegates to comparisonBuilder and functionBuilder
            Inputs = [];
            MaybeError = None;
            ApplyArg = fun(this, jpTerm) -> 
                match jpTerm with
                
                | JPTerm.OpenFilter ->
                    match this.Inputs with
                    | [] ->
                        ApplyArgResult.Incomplete {this with Inputs = [jpTerm]}
                    | _ -> 
                        let msg = sprintf "Error: expected one of Selector or functionName but got %A" jpTerm
                        ApplyArgResult.Error msg

                | JPTerm.FunctionName _functionName ->
                    ApplyArgResult.Error "Not implemented yet tbd tk"

                | _ -> 
                    match this.Inputs with
                    | h :: [] ->
                        let cb = comparisonFilterBuilder()
                        let fb' = cb.applyArg(h) 
                        match fb' with 
                        | ApplyArgResult.Incomplete fb ->
                                fb.applyArg(jpTerm) 
                        | ApplyArgResult.Complete _fb ->
                            let msg = sprintf "Error: Not expecting comparisonFilterBuilder to be complete%A" jpTerm
                            ApplyArgResult.Error msg
                    
                        | ApplyArgResult.Error msg ->
                            ApplyArgResult.Error msg
                    
                    | _ -> 
                        let msg = sprintf "Unexpected number of inputs in filterDelegator %d %A" this.Inputs.Length jpTerm
                        ApplyArgResult.Error msg

            MakeFunction = fun(inputs) ->   //this function is called via FunctionBuilder.makeFunction which passes in inputs list                
                printfn "This function should not be called.  Filter delegator should have delegated (we should return a result here tk)"
                fun _nodes -> []
        } 

    // the problem with these terms is that BinaryComparison is not a basic unit
    // in a way xpTerms are more fundamental even though they have builder info added
    // let inputs = argApplicationResult'.getInputs()

    // let parseConstant = (tryParseString) >=> tryParseNumber
    
    let rec parseExpression(expr: string) =
        // let rootOp = makeRootOp()
        // let opStack: Stack<BinaryOp> = Stack []
        // let rootStack = Stack.push rootOp opStacks
        // when we get to a where clause we either need to recurse
        // or have some other way of delineating the scope of the inner clause
        // or do we know that scope has  finished once we resolve a selector

        // prefix with / if not already starting with one - this rules out a pure constant tk
        // it requires that an XPath string alwaysbegins with a node selection
        // which may be fair enough
        let expr' =
            match expr[0..0] with
            | Eq @"/" -> expr
            | _ -> "/" + expr

        let tryEverything = 
                tryParseAxes 
            >=> tryParseElementSelection 
            >=> tryParseAttributeSelection 
            >=> tryParseWhereOpen
            >=> tryParseWhereClose
            >=> tryParseFunctionOpen
            >=> tryParseFunctionClose 
            >=> tryParseFunctionName
            // >=> tryParseFunctionContents 
            >=> tryParseComparisonOperator
            >=> tryParseConstant

        let rec f(acc: Result<list<JPTerm> * string, string>) =
            match acc with
            |Ok (terms, remaining) ->
                    match remaining.Length with
                    | Eq 0 -> acc
                    | _ -> 
                        match tryEverything(remaining) with
                        | Ok (Some terms', remaining') ->
                            let newTerms =
                                terms' |>
                                List.fold(fun accu i ->
                                    i :: accu
                                ) terms
                            f(Ok (newTerms, remaining'))
                        | Ok (None, remaining') ->
                            // let x = expr[expr.Length - remaining'.Length .. ]
                            let msg = sprintf "Unable to find a parse match for string %s" remaining'
                            Error msg
                        | Error err ->
                            Error err
            | Error err ->
                Error err

        let g:Result<list<JPTerm> * string, string> = f(Ok ([], expr'))
        match g with
        | Ok (terms, _remaining) ->
            let jpTerms =
                terms |>
                List.rev
            printfn "Successful parse"
            Ok jpTerms
        | Error err ->
            printfn "%s" err
            Error err



    let rec gatherTerms(input: string, values: list<Value * DataType>, binOps: list<BinaryOp>, expecting: Expecting) : Result<list<Value * DataType> * list<BinaryOp>, string> =    
        // this function does an initial pass through the expression, creating a list of terms
        Error "tbd"
    ()
    
    let testParseExpression(expr:string) = 
        let exprRes = parseExpression(expr) // these values are just placeholders though they are returned from processCalcTree
        printfn "%A" exprRes

    // parseConstant("'hello' there")
