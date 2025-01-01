namespace Parser

module ParserTypes =
    open Tweega.Shared.Types

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
    [<RequireQualifiedAccess>]
    type Number = 
    | Float64
    | Float32
    | Int64
    | Int32
    | Int16
    | Int8

    [<RequireQualifiedAccess>]
    type DataType = 
    | Numeric of Number
    | String
    | Boolean
    | Unknown
    | BadVal of string
    | DateTime
    | DateOffset




   // [<RequireQualifiedAccess>]
    type Constant = 
    | StringConst of string
    | NumericalConst of string
    // dates can be constants also
    // there will also be AF specific constants

    type ReturnType = DataType
    type DurationSeconds = int

    [<RequireQualifiedAccess>]
    type BinaryOp = {
        Operator: BinaryOperator;
        LHS: option<TypedTerm>;
        RHS: option<TypedTerm>;
    }

    // brackets, mult div,  plus, minus
    
    and [<RequireQualifiedAccess>] Conditional = {
        Predicate:TypedTerm;
        OnSuccess: TypedTerm;
        OnFail: TypedTerm;
    }

    and [<RequireQualifiedAccess>] Value = // values are indivisible and evaluate to a base type such as int
    | Field of string // label for a value in a record.  For AF, this would be an attribute in a collection of templated elements.
    | Constant of Constant // we could have an option of path here
    | Path of string // this would not be a thing in AF as path references from an analysis are always via a string builder attribute at the local level - this could be different for other systems
    | BinaryOpValue of BinaryOp // for bracketed expressions
    | Conditional of Conditional // Predicate, OnSuccess, OnFail
    // | Function of string * list<Value * DataType> // labelled bracketed expression
    | Function of string * ReturnType * list<TypedValue> // labelled bracketed expression
    | FixedDate of System.DateTime
    | TimeOffset of TimeUnit * int

    and [<RequireQualifiedAccess>] Term = 
    | Value of Value
    | BinaryOp of BinaryOp  // a binaryOp is a mappend and combines two things of the same type

    and TypedTerm = Term * DataType
    and TypedValue = Value * DataType

    [<RequireQualifiedAccessAttribute>]
    type QueueType = 
    | Input
    | Output
    | Constant of Constant
    // | Function  // we are now aiming to evaluate functions at the time controller level not binOp evaluator
    // so the binary operators do not need to evaluate functions.
    
    // if functions are identified in the inputs list, then we could resolve those there and then
    // before passing in - each function will have its own list of inputs which refrence one of
        // time
        // tag
        // previous expression result.

    type FunctionName = string

    // how are we going to handle conditionals? has this been done yet? I think so.  these are just variants on binary ops
    // might function args come from 'user' input? for the moment, assume that all inputs to this are constants and do not reference input values
    [<RequireQualifiedAccessAttribute>]
    type FunctionArg = 
    | Constant of Constant  // a function arg can be a constant such as a time period "*" or tasg name, but are bound into binaryOp functions
    | InputValue of InputValue

    and [<RequireQualifiedAccessAttribute>] InputValue = 
    | Field of FunctionName * DataType  // this is equivalent to Tag / Variable name (calculated in same analysis)
    | Function of FunctionName * ReturnType * list<FunctionArg>

    let noOp = Operator (NoOp, 0)
    let opPlus = Operator (Plus, 1)
    let opMinus = Operator (Minus, 1)
    let opModulo = Operator (Modulo, 1) 
    let opMultiply = Operator (Multiply, 2)
    let opDivide = Operator (Divide, 2)
    let opPower = Operator (Power, 3)
    let opEq = Comparator Equals
    let opGT = Comparator GreaterThan
    let opGTE = Comparator GreaterThanOrEquals
    let opLT = Comparator LessThan
    let opLTE = Comparator LessThanOrEquals

    type Mappend<'T> = ('T * 'T -> 'T) // change to T -> T -> T? tk
    type BinaryCalcOp = BinaryOperator * QueueType * QueueType  //make into a record?

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
