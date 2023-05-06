namespace Parser

module ParserTypes =

    type Precedence = int

    
    type ArithmeticSymbol = 
    | Plus
    | Minus
    | Multiply
    | Divide
    | NoOp
    | Power
    | Modulo

    type ComparatorSymbol = 
    | Equals
    | LessThan
    | GreaterThan
    | LessThanOrEquals
    | GreaterThanOrEquals

    type BinaryOperator = 
    | Operator of ArithmeticSymbol * Precedence
    | Comparator of ComparatorSymbol

    //type BinaryOperator = Symbol //* Precedence // precedence only makes sense for arithmetic operators? tk

    type DataType = 
    | Numeric
    | String
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
    | Tag of string // for the moment assume that tag type is always float
    | Constant of Constant
    | Path of string
    | BinaryOpValue of BinaryOp // for bracketed expressions
    | Function of string * list<TypedTerm> // labelled bracketed expression

    and Term = 
    | Value of Value
    | BinaryOp of BinaryOp  // a binaryOp is a monoid and combines two things of the same type

    and TypedTerm = Term * DataType

    [<RequireQualifiedAccessAttribute>]
    type DataQueue = 
    | Input
    | Output

    type OpFunc<'T> = ('T * 'T -> 'T) 
    type CalcOp<'T> = OpFunc<'T> * DataQueue * DataQueue  //make into a record?

    type ParseResult = 
        | ParseOK of option<string> * string //text matching re, remaining string to parse, accumulator
        | ParseError of string

        type Unary = 
            | Unary of BinaryOperator
            static member Combine(unaryA: Unary, unaryB: Unary) = 
                match (unaryA, unaryB) with
                | Unary (Operator (Plus, prec)), Unary (Operator (Plus, _)) -> Ok (Unary (Operator (Plus, prec)))
                | Unary (Operator (Minus, prec)), Unary (Operator (Minus, _)) -> Ok (Unary (Operator (Plus, prec)))
                | Unary (Operator (Plus, prec)), Unary (Operator (Minus, _)) -> Ok (Unary (Operator (Minus, prec)))
                | Unary (Operator (Minus, prec)), Unary (Operator (Plus, _)) -> Ok (Unary (Operator (Minus, prec)))
                | _ -> Error "Only Plus and Minus accepted as unary operators"
    
    type Expecting  =
    | BinOp
    | Val of Unary

    type IntegralPart = int
    type FractionPart = int
