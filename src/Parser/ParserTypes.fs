namespace Parser

module ParserTypes =

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
    | Operator of ArithmeticSymbol * Precedence
    | Comparator of ComparatorSymbol
    with 
        member this.OpToString() = 
            match this with 
            | Operator (aSym, _prec) ->
                match aSym with 
                | ArithmeticSymbol (sym, _) ->
                    match sym with 
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

    let noOp = Operator (ArithmeticSymbol (NoOp, true), 0)
    let opPlus = Operator (ArithmeticSymbol (Plus, true), 1)
    let opMinus = Operator (ArithmeticSymbol (Minus, false), 1)
    let opModulo = Operator (ArithmeticSymbol (Modulo, false), 1) 
    let opMultiply = Operator (ArithmeticSymbol (Multiply, true), 2)
    let opDivide = Operator (ArithmeticSymbol (Divide, false), 2)
    let opPower = Operator (ArithmeticSymbol (Power, false), 3)


    type OpFunc<'T> = ('T * 'T -> 'T) 
    type CalcOp<'T> = OpFunc<'T> * DataQueue * DataQueue  //make into a record?

    type ParseResult = 
        | ParseOK of option<string> * string //text matching re, remaining string to parse, accumulator
        | ParseError of string

        type Unary = 
            | Unary of BinaryOperator
            static member Combine(unaryA: Unary, unaryB: Unary) = 
                let g = opPlus
                match (unaryA, unaryB) with
                | Unary (Operator (ArithmeticSymbol (Plus, _), _)), Unary (Operator (ArithmeticSymbol (Plus, _),_)) -> Ok (Unary opPlus)
                | Unary (Operator (ArithmeticSymbol (Minus, _), _)), Unary (Operator (ArithmeticSymbol (Minus, _), _)) -> Ok (Unary opPlus)
                | Unary (Operator (ArithmeticSymbol (Plus, _), _)), Unary (Operator (ArithmeticSymbol (Minus, _), _)) -> Ok (Unary opMinus)
                | Unary (Operator (ArithmeticSymbol (Minus, _), _)), Unary (Operator (ArithmeticSymbol (Plus, _), _)) -> Ok (Unary opMinus)
                | _ -> Error "Only Plus and Minus accepted as unary operators"
    
    [<RequireQualifiedAccessAttribute>]
    type Expecting  =
    | BinOp
    | Val of Unary

    type IntegralPart = int
    type FractionPart = int
