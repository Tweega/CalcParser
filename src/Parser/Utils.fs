namespace Tweega
open Parser.ParserTypes

// this may need to be in its own project to be accessible to other projects that Server.fsproj
module Utils =
    
//should this be a util or go somewhere shared?
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
    
    let inline lift<'a, 'b, 'c> (bc: 'b ->'c) (ab: 'a -> 'b) : 'a -> 'c =
        fun (a:'a) ->
            a |> (ab >> bc)

    let inline tryUnbox<'a> (o:obj) =
        match o with
        | :? 'a as result -> Some (result)
        | _ -> None

    // this should be in a utils module
    let reverseString (input: string) =
        input |> Seq.rev |> Seq.toArray |> System.String


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

    let tryGetFirst<'T>(items: list<'T>) = 
        match items with 
        | [] -> None
        | h :: t -> Some h

    
    let (|Eq|_|) expected value =
        match expected = value with 
        | true -> Some ()
        | _ -> None

    let tryResolveBool (s:string) : option<bool> = 
        match System.Boolean.TryParse(s) with 
        | true, n -> Some n
        | _ -> None
        
    let tryResolveInt16 (s:string) =
        match System.Int16.TryParse(s) with 
        | true, n -> 
            Some (n |> (NumericValue.Int16 >> ResolvedValue.Numeric))
        | _ -> None
    
    let tryResolveInt32 (s:string) =
        match System.Int32.TryParse(s) with 
        | true, (n:int32) -> 
            Some (n |> (NumericValue.Int32 >> ResolvedValue.Numeric))
        | _ -> None

    let tryResolveInt64 (s:string) =
        match System.Int64.TryParse(s) with 
        | true, n ->
            Some (n |> (NumericValue.Int64 >> ResolvedValue.Numeric))
        | _ -> None

    let tryResolveFloat32 (s:string) =
        match System.Single.TryParse(s) with 
        | true, n ->
            Some (n |> (NumericValue.Float32 >> ResolvedValue.Numeric))
        | _ -> None
    
    
    let tryResolveFloat64 (s:string) =
        match System.Double.TryParse(s) with 
        | true, n -> 
            Some (n |> (NumericValue.Float64 >> ResolvedValue.Numeric))
        | _ -> None

    let tryResolveDate(format, provider) (s: string) : option<System.DateTime> =
        try 
            let ts = System.DateTime.ParseExact(s, format, provider)
            Some ts
        with 
            _ -> None

    let cons tail head = head :: tail

    let tryResolveNumber(numStr: string) =
        let numericParsers = [
            tryResolveFloat64
        ]
        List.AnyTry(numericParsers, numStr)



