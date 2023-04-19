namespace Parser

module ParserTypes =

    type Precedence = int

    type Symbol = 
    | Plus
    | Minus
    | Multiply
    | Divide
    | NoOp
    | Power
    | Modulo

    type BinaryOperator = Symbol * Precedence

    type DataType = 
    | Numeric
    | String
    | Unknown

    // | Boolean?

    let noOp = NoOp, 0
    let opPlus = Plus, 1
    let opMinus = Minus, 1
    let opModulo = Modulo, 1 
    let opMultiply = Multiply, 2
    let opDivide = Divide, 2
    let opPower = Power, 3

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
