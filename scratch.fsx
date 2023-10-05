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
        Elements: list<ElementNode>
    }  


open Thoth.Json.Net

let decodeDataType : Decoder<DataType> =
    Decode.string
    |> Decode.andThen (function
        | "Float" -> Decode.succeed Float
        | "Integer" -> Decode.succeed Integer
        | "String" -> Decode.succeed String
        | "Date" -> Decode.succeed Date
        | _ -> Decode.fail "Invalid DataType")


let listToMapDecoder (decoder: Decoder<(string * 'value) list>): Decoder<Map<string, 'value>> =
    decoder |> Decode.map Map.ofList

let rec decodeAttributeNode () : Decoder<AttributeNode> = 
    Decode.object
        (fun get ->
            { Name = get.Required.Field "Name" Decode.string
              Value = get.Required.Field "Value" Decode.string
              AttributeMap = get.Required.Field "AttributeMap" (listToMapDecoder (Decode.keyValuePairs (decodeAttributeNode ())))
              DataType = get.Required.Field "DataType" decodeDataType })

let rec decodeElementNode () : Decoder<ElementNode> = 
    Decode.object
        (fun get ->
            { Name = get.Required.Field "Name" Decode.string
              AttributeMap = get.Required.Field "AttributeMap" (listToMapDecoder (Decode.keyValuePairs (decodeAttributeNode ())))
              Elements = get.Required.Field "Elements" (Decode.list (decodeElementNode ())) })

let decodeFromJson jsonString =
    Decode.fromString (decodeElementNode ()) jsonString

// [<EntryPoint>]
// let main argv =

let testString = """
{
    "Name": "RootElement",
    "AttributeMap": {
        "attr1": {
            "Name": "attr1",
            "Value": "rootValue1",
            "AttributeMap": {},
            "DataType": "String"
        },
        "attr2": {
            "Name": "attr2",
            "Value": "rootValue2",
            "AttributeMap": {},
            "DataType": "Integer"
        }
    },
    "Elements": [
        {
            "Name": "ChildElement1",
            "AttributeMap": {
                "childAttr1": {
                    "Name": "childAttr1",
                    "Value": "childValue1",
                    "AttributeMap": {},
                    "DataType": "Date"
                },
                "childAttr2": {
                    "Name": "childAttr2",
                    "Value": "childValue2",
                    "AttributeMap": {},
                    "DataType": "Float"
                }
            },
            "Elements": [
                {
                    "Name": "GrandChildElement",
                    "AttributeMap": {
                      "attr1x": {
                          "Name": "attr1x",
                          "Value": "rootValue1",
                          "AttributeMap": {},
                          "DataType": "String"
                      },
                      "attr2x": {
                          "Name": "attr2x",
                          "Value": "rootValue2",
                          "AttributeMap": {},
                          "DataType": "Integer"
                      }
                  },
                    "Elements": []
                }
            ]
        }
    ]
}

"""
    
match decodeFromJson testString with
| Ok res -> printfn "%A" res
| Error e -> printfn "%s" e
0

type Reader<'r, 't> = Reader of ('r -> 't)


let run (Reader r) = r // run ~ flatten
let map f (Reader r) = Reader(r >> f) //map is here compose with the Reader wrapper
let bind secondFunction (Reader firstFunction) = 
    Reader (fun  env ->  
        // we have 2 functions, both of which want the same environment variable
        // we call the first function that wants it

        let outputFromFirstFunction = firstFunction env

        //the second function wants the output from the first function and also the environment variable
        let outputOfSecondFunction_Reader = outputFromFirstFunction secondFunction env

        let outputOfSecondFunction = run outputOfSecondFunction_Reader  //run just unwraps function from Reader type

        outputOfSecondFunction

        //so this is function composition with an extra shared variable that threads through the chain
)
let apply (Reader f) (Reader x) =
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

let attributeLens2 (key: string) = 
    let getter:ElementNode -> AttributeNode = 
        fun e -> Map.find key e.AttributeMap
    let setter: AttributeNode -> ElementNode = 
        fun (v:AttributeNode) (e:ElementNode) -> 
            let newEl: ElementNode = { e with AttributeMap = Map.add key v e.AttributeMap }
            newEl

    // let f: ElementNode -> ElementNode  = 
    lens getter
        //  (fun v e -> { e with AttributeMap = Map.add key v e.AttributeMap })

