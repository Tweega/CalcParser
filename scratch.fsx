#r "src/Parser/bin/Debug/net6.0/Parser.dll"

let quot = (string) '\u0022'    
let quote(s:string) = 
  quot + s + quot

let simple = Parser.CalcParser.testParseExpression("1 + 2")
simple([123;321])
let moreComplex = Parser.CalcParser.testParseExpression("1 + 2 * 3")
moreComplex([2;3;1])
  

let withBracket = Parser.CalcParser.testParseExpression("1 * (2 + 3)")
withBracket([2;3;1])
  

let moreComplexEval = Parser.CalcParser.testParseExpression("(1 + 2) + (3 * 4) * 5")
moreComplexEval([1;2;3;4;5])
moreComplexEval([21;2;4;7;13])

let nestedBrackets = Parser.CalcParser.testParseExpression("(1 + (2 * 3))")
nestedBrackets([2;3;1])
nestedBrackets([4;5;6])

let jollyNested = Parser.CalcParser.testParseExpression("5 * ((1 + 2) * (3 + 4))")
jollyNested([1;2;3;4;5])
jollyNested([11;21;23;14;5])

let superNested = Parser.CalcParser.testParseExpression("(105 / (5 * ((1 + 2) * (3 + 4))))")
superNested([1;2;3;4;5;105])

let s = "tagTot('CDT158') + tagAvg('Sinusoid', " + quote("*-1d") + ", " + quote("*") + ")"
let res = Parser.CalcParser.parseExpression(s)
match res with 
| Ok (t, _dt) -> 
  let expr = Parser.CalcParser.expressionFromTerm(t)
  printfn "%s" expr
| Error err -> 
  printfn "%s" err
  //> let expr = Parser.CalcParser.parseExpression("1 + 2")

  // Parser.CalcParser.createCalcDef(expr)
  // let plusEval = Parser.CalcParser.testParseExpression("1 + 2")


let hi = ""
hi[0..0]

#r "nuget: Thoth.Json.Net"

type DataType =
    | Float
    | Integer
    | String
    | Date

type AttributeNode = 
    {
        Name: string;
        Value: string;
        AttributeMap: Map<string, AttributeNode>
        DataType: DataType;
    }  

and ElementNode = 
    {
        Name: string;
        AttributeMap: Map<string, AttributeNode>
        ElementMap: Map<string, ElementNode>
    }  


let example = """
{
    "Name": "RootElement",
    "Attributes": 
        [
            {
                "Name": "attr1",
                "Value": "rootValue1",
                "Attributes": [],
                "DataType": "String"
            },
            {
                "Name": "attr2",
                "Value": "rootValue2",
                "Attributes": [],
                "DataType": "Integer"
            }
        ],
    
    "Elements": [
        {
            "Name": "ChildElement1",
            "Attributes": [],
            "Elements": []
        }
    ]
}

"""
open Thoth.Json.Net

let decodeDataType: Decoder<DataType> =
    Decode.string
    |> Decode.andThen (function
        | "Float" -> Decode.succeed Float
        | "Integer" -> Decode.succeed Integer
        | "String" -> Decode.succeed String
        | "Date" -> Decode.succeed Date
        | _ -> Decode.fail "Invalid DataType")

let keyFuncA = (fun (item: AttributeNode) -> item.Name)
let keyFuncE = (fun (item: ElementNode) -> item.Name)
let rec decodeAttributeNode(): Decoder<AttributeNode> =
    Decode.object
        (fun get ->
            {
                Name = get.Required.Field "Name" Decode.string;
                Value = get.Required.Field "Value" Decode.string;
                AttributeMap = get.Required.Field "Attributes" listToMapDecoderA;
                DataType = get.Required.Field "DataType" decodeDataType;
            }
        )

and decodeElementNode(): Decoder<ElementNode> =
    Decode.object
        (fun get ->
            {
                Name = get.Required.Field "Name" Decode.string;
                AttributeMap = get.Required.Field "Attributes" listToMapDecoderA;
                ElementMap = get.Required.Field "Elements" listToMapDecoderE;
            }
        )

and genericListToMapDecoder keyFunc itemDecoder: Decoder<Map<string, 'T>> =
    Decode.list itemDecoder
    |> Decode.map (fun items ->
        items
        |> List.map (fun item -> keyFunc item, item)
        |> Map.ofList)


and listToMapDecoderA = genericListToMapDecoder keyFuncA (decodeAttributeNode())
and listToMapDecoderE = genericListToMapDecoder keyFuncE (decodeElementNode())

let decodeFromJson jsonString =
    Decode.fromString (decodeElementNode()) jsonString

// [<EntryPoint>]
// let main argv =

let testString = """
{
    "Name": "RootElement",
    "Attributes": [
        {
            "Name": "attr1",
            "Value": "rootValue1",
            "Attributes": [],
            "DataType": "String"
        },
        {
            "Name": "attr2",
            "Value": "rootValue2",
            "Attributes": [],
            "DataType": "Integer"
        }
    ],
    "Elements": [
        {
            "Name": "ChildElement1",
            "Attributes": [
                {
                    "Name": "childAttr1",
                    "Value": "childValue1",
                    "Attributes": [],
                    "DataType": "Date"
                },
                {
                    "Name": "childAttr2",
                    "Value": "childValue2",
                    "Attributes": [],
                    "DataType": "Float"
                }
            ],
            "Elements": [
                {
                    "Name": "GrandChildElement",
                    "Attributes": 
                        [
                            {
                                "Name": "attr1x",
                                "Value": "rootValue1",
                                "Attributes": [],
                                "DataType": "String"
                            },
                            {
                                "Name": "attr2x",
                                "Value": "rootValue2",
                                "Attributes": [],
                                "DataType": "Integer"
                            }
                        ],
                    "Elements": []
                }
            ]
        }
    ]
}

"""
    
type Reader<'r, 't> = Reader of ('r -> 't)


let runr (Reader r) = r // run ~ flatten
let mapr f (Reader r) = Reader(r >> f) //map is here compose with the Reader wrapper
let bindr secondFunction (Reader firstFunction) = 
    Reader (fun  env ->  
        // we have 2 functions, both of which want the same environment variable
        // we call the first function that wants it

        let outputFromFirstFunction = firstFunction env

        //the second function wants the output from the first function and also the environment variable
        let outputOfSecondFunction_Reader = outputFromFirstFunction secondFunction env

        let outputOfSecondFunction = runr outputOfSecondFunction_Reader  //run just unwraps function from Reader type

        outputOfSecondFunction

        //so this is function composition with an extra shared variable that threads through the chain
)
let applyr (Reader f) (Reader x) =
    Reader (fun a -> f a ((x: _ -> 'T) a))


let apply2 (Reader firstReaderFunction_is_a_to_b_to_c) (Reader secondReaderFunction_is_a_to_b) =
    Reader (fun a ->
        let outputOfFirstReaderFunction_is_b_to_c = firstReaderFunction_is_a_to_b_to_c a // applying f to a produces a function b -> c 
        let outputOfSecondReaderFunction_is_b = secondReaderFunction_is_a_to_b a // second function is a -> b
        let finalOutput_is_c = outputOfFirstReaderFunction_is_b_to_c outputOfSecondReaderFunction_is_b
        finalOutput_is_c 
        // so we essentially take function a -> b -> c, apply it to a giving us b -> c
        // we also apply th a to second function which is a -> b and get a b
        // the output from the first function application is b->c so can take our b giving us a c

        // if I have a function that adds 2 numbers (+) which is  a specialised a->b->c 
        // and a function that doubles x : (*) 2

        // then i end up with a function that takes a, say 5, applies this to the double function and to the add function
        // so we have (+) 5 as ouput from first (call it first derived function)
        // and 10 as output from second
        // then we apply 10 to the first derived function which gives us (+) 5 10 =  15

    )

// let apply3 (Reader (firstReaderFunction_is_a_and_env_to_b: 'a -> 'env -> 'b)) (Reader (secondReaderFunction_is_b_and_env_to_c: 'b -> 'env -> 'c)) =
//     Reader (fun a ->
//         let outputOfFirstReaderFunction_is_b_to_c = firstReaderFunction_is_a_to_b_to_c a // applying f to a produces a function b -> c 
//         let outputOfSecondReaderFunction_is_b = secondReaderFunction_is_a_to_b a // second function is a -> b
//         let finalOutput_is_c = outputOfFirstReaderFunction_is_b_to_c outputOfSecondReaderFunction_is_b
//         finalOutput_is_c 
//     )

// applying a number to 2 functions and then applying output of one application to another is an odd thing to do
        // but if we have 2 functions  that both need a dictionary, say, or a library of functions
        // then it makes more sense.  Or in this case a passmark integer.  

        //If I have a list of students and scores in language tests as tuples indicating 
        // strength in reading and writing as a tuple (readingStrengh, writingstrength) 
        // we might have a passmark that is common to both abilities; the passmark being the environment variable
let langScores = ["AB", (1, 2); "HG", (3,2); "LJ", (4,4)]

//what i want is then a function that takes passmark and passes it to first function
// the output of that is passed to the second with environment (passmark)

let scoreCompose(f1:list<string * (int * int)> -> int -> list<string * (int * int)>, f2:list<string * (int * int)> -> int -> list<string * (int * int)>) =
    fun (scores: list<string * (int * int)>) ->
        fun (passmark: int) ->  //this could be moved up into the previous line
                let f1Scores = f1 scores passmark 
                f2 f1Scores passmark

let canReadWell (scores:list<string * (int * int)>)(passmark:int) =
    scores |>
    List.filter(fun i -> 
        (i |> (snd >> fst)) > passmark
    )

let canWriteWell (scores:list<string * (int * int)>)(passmark: int) =
    scores |>
    List.filter(fun i -> 
        (i |> (snd >> snd)) > passmark
    )


let canReadAndWrite = scoreCompose(canReadWell, canWriteWell)
let goodAtReadingAndWriting = canReadAndWrite langScores 3

//to generalise
let compose'(f1: 'a-> 'env -> 'b) (f2: 'b -> 'env -> 'c) =
    fun(a: 'a) (env: 'env) ->
        let f1Out = f1 a env
        f2 f1Out env

let canReadAndWrite' = compose' canReadWell canWriteWell
let goodAtReadingAndWriting' = canReadAndWrite' langScores 3

// to use a Reader wrapper
let rCompose(Reader f1)(Reader f2) =
    Reader (fun(a: 'a) (env: 'env) ->
        let f1Out = f1 a env
        f2 f1Out env
        )

let applyOrig (Reader (f: 'env -> 'a -> 'b)) (Reader (x: 'env -> 'b -> 'c)) =
    Reader (fun env a -> (f env a) |> (x env))

let canReadAndWrite'' = rCompose (Reader canReadWell) (Reader canWriteWell)
let goodAtReadingAndWriting'' = (run canReadAndWrite'') langScores 3


#r "nuget: FSharpPlus, 1.4.1"
open FSharpPlus.Lens

let gg key = (fun e -> Map.find key e.AttributeMap) 

// let attributeLens key = 
//     lens (fun e -> Map.find key e.AttributeMap) 
//          (fun v e -> { e with AttributeMap = Map.add key v e.AttributeMap })


open FSharpPlus
open FSharpPlus.Data

// val inline lens:
//    getter: ('s -> 'a) ->  1) the getter takes a container object ('s) and finds the part that needs updating ('a)
//    setter: ('s -> 'b -> 't) -> 1a) we also apply the container object 's to the setter. This gives us a 'b -> 't
//    f     : ('a -> ^F<'b>) -> 2) the transformer function, transforms the 'a into an instance of a Functor on 'b
// the partially applied setter function is handed to the Map function of the functor giving us an F<'t>
//    s     : 's //  start theball rolling by providing the container object 's
//         -> 'F<'t>
//  the view function unwraps F<'t> to return 't
// the  setl function 


// val inline lens:
//    getter: ('container -> 'part) ->  1) the getter takes a container object ('s) and finds the part that needs updating ('a)
//    setter: ('container -> 'newValue -> 'partParent) -> 1a) we also apply the container object 's to the setter. This gives us a 'b -> 't
//    f     : ('part -> ^F<'newValue>) -> 2) the transformer function, transforms the 'a into an instance of a Functor on 'b
// the partially applied setter function is handed to the Map function of the functor giving us an F<'t>
//    s     : 'container //  start theball rolling by providing the container object 's
//         -> 'F<'partParent>
//  the view function unwraps F<'t> to return 't
// the  setl function provides Functor Identity
// when you call view providing getter and setter



let attributeLens key =
    let getter (e: ElementNode) = Map.tryFind key e.AttributeMap
    let setter (e: ElementNode) (v: AttributeNode option) =
        match v with
        | Some value -> { e with AttributeMap = Map.add key value e.AttributeMap }
        | None -> { e with AttributeMap = Map.remove key e.AttributeMap }
    lens getter setter



let getThroughLensA (lens: ElementNode -> AttributeNode option) (s: ElementNode) =
    lens s

let setThroughLensA (lens: 'T -> 'U option) (setter: 'T -> 'U -> 'T) (s: 'T) (value: 'U) =
    match lens s with
    | Some _ -> setter s value
    | None -> s


let setThroughLens (lens: ElementNode -> ElementNode) (element: ElementNode) (newAttribute: AttributeNode) : ElementNode =
    let transformedElement = lens element
    let updatedMap = Map.add newAttribute.Name newAttribute transformedElement.AttributeMap
    { transformedElement with AttributeMap = updatedMap }


let attributeLens key  =
    let getter (e: ElementNode) = Map.tryFind key e.AttributeMap
    let setter (e: ElementNode) (v: AttributeNode option) =
        match v with
        | Some value -> { e with AttributeMap = Map.add key value e.AttributeMap }
        | None -> e
    lens getter setter
    
let elementLens key el =
    let getter (e: ElementNode) = Map.tryFind key e.ElementMap
    let setter (e: ElementNode) (v: ElementNode option) =
        match v with
        | Some value -> { e with ElementMap = Map.add key value e.ElementMap }
        | None -> e
    lens getter setter


let myLens = attributeLens "attr1"

let value = Lens. myLens elementNode
let updatedElementNode = Lens.setl myLens newAttributeNode elementNode


let myLens = attributeLens "attr1"
let updatedElement = setThroughLensA myLens element { Name = "attr1"; Value = "new value"; AttributeMap = Map.empty; DataType = String }



    

match decodeFromJson testString with
| Ok (rootNode:ElementNode) ->
    // let maybeAttribute = attributeLens "attr1" rootNode
    let attribute = getThroughLens myLens rootNode
    match maybeAttibute with
    | Some attr ->
        printfn "Attribute found"

    | None ->
        printfn "Attribute not found"
    printfn "%A" g
| Error e -> printfn "%s" e




type Postcode = Postcode of string

type Address =
    { HouseNumber: string
      Postcode: Postcode }

type CreditCard =
    { Number: string
      Expiry: string
      Cvv: string
      Address: Address }

type User = { CreditCard: CreditCard }


module Address =
    let setPostcode (transformer: Postcode -> Postcode) address =
        { address with
              Postcode = address.Postcode |> transformer }

module CreditCard =
    let setAddress (transformer: Address -> Address) card =
        { card with
              Address = card.Address |> transformer }

module User =
    //each update function takes a function  that can operate on a part of a structure,returning updated version of that structure
    let setCreditCard (transformer: CreditCard -> CreditCard) user =
        { user with
              CreditCard = user.CreditCard |> transformer }


let setCreditCardPostcode (postcode: Postcode): User -> User =
    let constPostCode =
        (Address.setPostcode
        >> CreditCard.setAddress
        >> User.setCreditCard)

    let a:(Postcode -> Postcode) -> Address-> Address = Address.setPostcode
    
        
    constPostCode (fun _ -> postcode)


let tuple = (1,2)
let l = tuple.to