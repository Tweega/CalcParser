namespace Tweega.Shared
open System.Text.RegularExpressions
open System
open Tweega.Shared.Types

// this may need to be in its own project to be accessible to other projects that Server.fsproj
module Utils =

    type Microsoft.FSharp.Collections.List<'a> with
        static member Join (lists: list<list<'T>>) =
            //collapses a list of list one level
            let emptyList: list<'T> = []

            lists
            |> List.fold(fun accListT listT ->
                listT |>
                List.fold(fun accT t -> t :: accT) accListT
            ) emptyList
            |> List.rev

        static member Bind(aList, fab) =
            aList |>
            List.map fab
            |> List.Join

        static member AllOf (predicate: 'T -> bool, ts:list<'T>)  =
            //what should an empty list evaluate to? currently returns true  could  have a default passed in
            let rec allOf(ts':list<'T>)  =
                match ts' with
                | h :: t ->
                    match predicate h with
                    | true -> allOf t
                    | false -> false
                | _ -> true
            allOf ts
    
        static member AnyOf (predicate: 'T -> bool, ts:list<'T>)  =
            //what should an empty list evaluate to? currently returns false  could  have a default passed in
            let rec anyOf(ts':list<'T>)  =
                match ts' with
                | h :: t ->
                    match predicate h with
                    | false -> anyOf t
                    | true -> true
                | _ -> false
            anyOf ts
    
    
        static member AnyTry (tries: list<'T -> option<'U>>, t: 'T)  =
            let rec anyTry(tries': list<'T -> option<'U>>)  =
                match tries' with
                | try' :: tail ->
                    match try' t with
                    | None -> anyTry tail
                    | someT -> someT
                | _ -> None
            anyTry tries
    
    let toConsole(msg: string) =
        printfn "%s" msg
        
    // refactor to akka.net?
    let createMailbox<'MsgType, 'State>(initialState: 'State) (handler: 'MsgType -> 'State -> 'State) =
        let mailbox = MailboxProcessor<'MsgType>.Start(fun agent ->
            let rec loop (state: 'State) = async {
                // Asynchronously wait for the next message
                let! msg = agent.Receive()
                let newState = handler msg state //newVal could be recorded on the state

                return! loop newState
            }

            // Start the body with initial values
            loop initialState)

        //pass back a function through which to access this data source
        fun(msg: 'MsgType) ->
            mailbox.Post(msg)


    let createMailboxWithSelf<'MsgType, 'State>(initialState: 'State) (handler: ('MsgType * ('MsgType -> unit) -> 'State -> 'State)) =
        let mailbox = MailboxProcessor<'MsgType>.Start(fun agent ->
            let rec loop (state: 'State) = async {
                // Asynchronously wait for the next message
                let! msg = agent.Receive()
                let newState = handler (msg, agent.Post) state

                return! loop newState
            }

            // Start the body with initial values
            loop initialState)

        //pass back a function through which to access this data source
        fun(msg: 'MsgType) ->
            mailbox.Post(msg)


    let combinerPrep = fun x -> [x]

    let mutable tempID = 1111

    let getIDWithRoot(root: string) =
        tempID <- tempID + 1
        sprintf "%s_%d" root tempID

    
    let getTempID() =
        tempID <- tempID + 1
        tempID

    let mutable portID = 9025
    let getFreePort() =
        portID <- portID + 1
        portID


    // rename to join lists
    let join (lists: list<list<'T>>) =
        //collapses a list of list one level
        let emptyList: list<'T> = []

        lists
        |> List.fold(fun accListT listT ->
            listT |>
            List.fold(fun accT t -> t :: accT) accListT
        ) emptyList
        |> List.rev

    // searching on key, if handler is found in map, forwards the message to that handler
    let forwardMessage = fun (f: MessageHandler<'Msg> -> unit) ((key: 'Key, map: Map<'Key, MessageHandler<'Msg>>)) (caller: string) ->
        match map.TryFind key with
        | Some msgHandler ->
            f msgHandler
        | None -> 
            toConsole( sprintf "Unable to find message handler from map in forwardMessage: %s, %A" caller key )

    let tryForwardMessage = fun (onSuccess: MessageHandler<'Msg> -> unit) (onFailure: unit -> unit) ((key: 'Key, map: Map<'Key, MessageHandler<'Msg>>)) (caller: string) ->
        match map.TryFind key with
        | Some msgHandler ->
            onSuccess msgHandler
        | None -> 
            toConsole( sprintf "Unable to find message handler from map in forwardMessage: %s, %A" caller key )  //how to handle streamwriter not found? tk
            onFailure()

    let inline lift<'a, 'b, 'c> (bc: 'b ->'c) (ab: 'a -> 'b) : 'a -> 'c =
        fun (a:'a) ->
            a |> (ab >> bc)


    let inline always<'a> (a: 'a) : _ -> 'a =
        fun (_) -> a

    let inline tryUnbox<'a> (o:obj) =
        match o with
        | :? 'a as result -> Some (result)
        | _ -> None


    [<RequireQualifiedAccess>]
    type Duration =
        | Seconds
        | Minutes
        | Hours
        | Days

    let getTicks (duration: Duration, n: int) =
        let seconds =
            match duration with
            | Duration.Seconds -> n
            | Duration.Minutes -> n * 60
            | Duration.Hours -> n * 60 * 60
            | Duration.Days -> n * 60 * 60 * 24

        int64 seconds * (int64 (10f ** 7f))

    let getDuration (duration: Duration, ticks: int64) =
        let seconds = int64 ticks / (int64 (10f ** 7f))
        match duration with
        | Duration.Seconds -> (float32) seconds
        | Duration.Minutes -> (float32) seconds / 60f
        | Duration.Hours -> (float32) seconds / 60f / 60f
        | Duration.Days -> (float32) seconds / 60f / 60f / 24f

    let (|IsValid|_|) bln = 
        match bln with 
        | true -> Some ()
        | false -> None

    let tryGetFirst<'T>(tokens: list<'T>) = 
        match tokens with 
        | [] -> None
        | h :: t -> Some h

    let getFirstFailureReason(failures: list<FailureReason>) =
        match tryGetFirst(failures) with
        | Some f -> f
        | None -> OtherReason "No failure reason supplied"

    let (|Exists|_|) = Map.tryFind

    let (|IsTruex|_|) pred x =
        if pred x then Some () else None

    let (|IsTrue|_|) x =
        if x = true then Some () else None

    
    let (|Eq|_|) expected value =
        match expected = value with 
        | true -> Some ()
        | _ -> None 

    let (|HasValue|_|) value =
        match value with 
        | Some v -> Some v
        | _ -> None 

    let parseBool (s:string) : option<bool> = 
        match System.Boolean.TryParse(s) with 
        | true, n -> Some n
        | _ -> None
        
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

    let parseDate(format, provider) (s: string) : option<System.DateTime> =
        try 
            let ts = System.DateTime.ParseExact(s, format, provider)
            Some ts
        with 
            _ -> None

    let cons tail head = head :: tail

    let noOp = fun _x -> ()

    //https://stackoverflow.com/questions/51828141/string-representation-of-f-function-signature
    let (|TFunc|_|) (typ: Type) =
        if typ.IsGenericType && typ.GetGenericTypeDefinition () = typeof<int->int>.GetGenericTypeDefinition () then
            match typ.GetGenericArguments() with
            | [|targ1; targ2|] -> Some (targ1, targ2)
            | _ -> None
        else
            None
    
    let rec getSig (typ: Type) =
        match typ.IsGenericParameter with
        | IsTrue -> toConsole( "IsGenericParameter: true")
        | _ -> toConsole( "IsGenericParameter:false")
        match typ.ContainsGenericParameters with
        | IsTrue -> toConsole( "ContainsGenericParameters: true")
        | _ -> toConsole( "ContainsGenericParameters:false")
        match typ.IsNested with
        | IsTrue -> toConsole( "IsNested: true")
        | _ -> toConsole( "IsNested:false")
        match typ.GenericTypeArguments.Length > 0 with
        | IsTrue -> 
            toConsole( "GenericTypeArguments: true")
            toConsole( sprintf "GenericTypeArguments: %A"  typ.GenericTypeArguments)
        | _ -> toConsole( "GenericTypeArguments:false")
        match typ.IsConstructedGenericType with
        | IsTrue -> toConsole( "IsConstructedGenericType: true")
        | _ -> toConsole( "IsConstructedGenericType:false")
        match typ.IsGenericType with
        | IsTrue -> toConsole( "IsGenericType: true")
        | _ -> toConsole( "IsGenericType:false")
        match typ.IsGenericTypeDefinition with
        | IsTrue -> toConsole( "IsGenericTypeDefinition: true")
        | _ -> toConsole( "IsGenericTypeDefinition:false")

        match typ = typeof<string> with
        | IsTrue -> toConsole( "typ = typeof<string: true")
        | _ -> toConsole( "typ = typeof<string:false")

        match 1 = 2 with
        | IsTrue -> toConsole( "1=2: true")
        | _ -> toConsole( "1=2 :false")
        

    let rec getTypeStr (typ: Type) =
        match typ with
        | TFunc (TFunc(_, _) as tfunc, t) -> sprintf "(%s) -> %s" (getTypeStr tfunc) (getTypeStr t)
        | TFunc (t1, t2) -> sprintf "%s -> %s" (getTypeStr t1) (getTypeStr t2)
        | typ when typ = typeof<int> -> "int"
        | typ when typ = typeof<string> -> "string"
        | typ when typ.GenericTypeArguments.Length > 0 ->             
            let openBracket = sprintf "<" 
            let closeBracket = ">"
            let body  =
                typ.GenericTypeArguments |>
                    Array.fold(fun acc gt ->
                        let j = getTypeStr gt 

                        acc + j
                    ) ""

            let rePattern  = "[A-Za-z]{1}[A-Za-z0-9]*"
            let nameMatch  = Regex.Match(typ.Name, rePattern)
            let name =
                match nameMatch.Success with
                | true ->
                    nameMatch.Value
                | false ->
                    typ.Name

            name +  openBracket +  body  + closeBracket
        | typ -> 
            getSig(typ)
            sprintf "ff%s"  (string typ)

    let rec zip (a, b) =
        match (a, b) with
        | ha :: ta, hb :: tb -> (ha, hb) :: zip (ta, tb)
        | _, _ -> []

    let flattenTaggedKVPs(taggedKVPs: list<TaggedKVPs>) =
        taggedKVPs |>
        List.fold (fun acc {Tag=_tag; KVPs = kvps} ->
            kvps |>
            List.fold(fun acc' i->
                (i.Key, i.Value) :: acc'
            ) acc
        ) []

    let flattenTaggedOptionsMap(taggedKVPs: Map<string,Map<string,string>>) =
        taggedKVPs |> 
        Map.fold (fun acc _tag m ->
            m |>
            Map.fold(fun acc' k  v ->
                (k, v) :: acc'
            ) acc
        ) []

    let kvpsToMap(options: list<KVP>) =
        options |>
        List.fold(fun acc' i->
            Map.add i.Key i.Value acc'
        ) Map.empty
                
    let trimString(s: string) =
        s.Trim()

    let tokeniseOn(splitOn: string) (s:string) = //splitOn is reg exp
        Regex.Split(s, splitOn)
        |> List.ofArray
        |> List.map trimString



    let tokenise(s:string) =
        let sTrim = s.Trim()
        match sTrim.Length with 
        | Eq 0 -> []
        | _ ->
            Regex.Split(sTrim, "\s+")
            |> List.ofArray

    let (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern) in
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ]) else None

    let (|StartsWith|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some()
        else
            None
        
    let inline stringf format (x : ^a) = 
        (^a : (member ToString : string -> string) (x, format))
    
    let optionFromPredicate pred x =
        if pred x then Some () else None

 
    let concatLists(listA: list<'T>, listB: list<'T>) : list<'T> =
        List.fold(fun acc t -> t :: acc) listA listB

    let mergeLists(lists: list<list<'T>>) : list<'T> =
        let rec joinLists(l: list<'T>, lists: list<list<'T>>) : list<'T> =
            match lists with
                | [] -> l            
                | h :: rest ->
                    let joined = concatLists(l, h)
                    joinLists(joined, rest)

        match lists with
            | [] -> []
            | h :: t -> joinLists(h, t)

    // https://stackoverflow.com/questions/16706047/cut-a-list-by-index-n-in-f
    let partitionList (n, xs) =
        let rec aux = function
            | 0, xs, ys -> List.rev xs, ys
            | n, xs, y :: ys -> aux (n - 1, y :: xs, ys)
            | _ -> failwith "invalid arguments"
        aux (n, [], xs)


    let inline delayed f a = fun () -> f(a)


    let reverseString (input: string) =
        input |> Seq.rev |> Seq.toArray |> System.String

    
    let tryResolveBool (s:string) : option<bool> = 
        match System.Boolean.TryParse(s) with 
        | true, n -> Some n
        | _ -> None
        
    let tryResolveInt16 (s:string) =
        match System.Int16.TryParse(s) with 
        | true, n -> 
            Some (n |> (NumericValue.Int16))
        | _ -> None
    
    let tryResolveInt32 (s:string) =
        match System.Int32.TryParse(s) with 
        | true, (n:int32) -> 
            Some (n |> (NumericValue.Int32))
        | _ -> None

    let tryResolveInt64 (s:string) =
        match System.Int64.TryParse(s) with 
        | true, n ->
            Some (n |> (NumericValue.Int64))

//            Some ((float) n |> (LiftedValue.Numeric))
        | _ -> None

    let tryResolveFloat32 (s:string) =
        match System.Single.TryParse(s) with 
        | true, n ->
            Some (n |> (NumericValue.Float32))

//            Some ((float) n |> (LiftedValue.Numeric))
        | _ -> None
    
    
    let tryResolveFloat64 (s:string) =
        match System.Double.TryParse(s) with 
        | true, n -> 
            Some (n |> (NumericValue.Float64))

//            Some (n |> LiftedValue.Numeric)
        | _ -> None

    let tryResolveDate(format, provider) (s: string) : option<System.DateTime> =
        try 
            let ts = System.DateTime.ParseExact(s, format, provider)
            Some ts
        with 
            _ -> None

    let tryResolveNumber(numStr: string) =
        let numericParsers = [
            tryResolveInt16;
            tryResolveInt32;
            tryResolveInt64;
            tryResolveFloat32;
            tryResolveFloat64;
        ]
        List.AnyTry(numericParsers, numStr)