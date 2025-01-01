namespace Tweega.Shared
open System

module Types =
    type TagAlias = string
    type Timestamp = DateTime
    type ClientRef = string
    type ServerRef = string


    type TimeSeriesValue<'T> = {
        Timestamp: Timestamp
        Value: 'T
    }

    type TaggedValues<'T> = {
        Tag: string
        Values: list<'T>
    }
    type KVP = { Key:string; Value:string }

    type TaggedKVPs = {
        Tag: string;// this will actually be the function name, possibly qfn - for each function that we want,these are the options. tk
        KVPs: list<KVP>;
    }

    type MessageHandler<'Msg> = 'Msg -> unit
    type Subscriber<'T> = 'T -> unit
    type Generator<'T, 'U> = 'T -> 'U

    // Credentails can be extended to include no credentials, impersonated etc. tk
    [<RequireQualifiedAccess>]
    type UserCredentials =
        | LoggedOnUser
        | Credentials of list<KVP>   //KVPs will end up being some kind of certificate
        | Ignore


    type CxnInfo = {
        CxnOptions: list<KVP>
        UserCredentials: UserCredentials
    }

    type CancelledReason =
        | JustBecause of string

    type FailureReason =
            | ExecutionError of string
            | Cancelled of CancelledReason
            | AuthenticationError of string
            | AccessError of string
            | OtherReason of string

    type ResourceID = int

    // type MainstreamRouter =  (StreamType * obj) -> unit
    type Either<'Left, 'Right> =
    | Left of  'Left
    | Right of 'Right



    

    // find a new home for these - not STTEmulator
    type Field = KVP
    type Record = TaggedKVPs
    type RecordList = list<Record>
    
    [<RequireQualifiedAccess>]
    type ParseStatusUI =
    | CompletedRecord of Record
    | RecordInProgress of list<KVP>
    | RecordError of string
    
    [<RequireQualifiedAccess>]
    type NumericValue = 
    | Float64 of float
    | Float32 of float32
    | Int64 of int64
    | Int32 of int32
    | Int16 of int16
    | Int8 of int8

    [<RequireQualifiedAccess>]
    type TimeUnit =
    | Year
    | Month
    | Week
    | Day
    | Hour
    | Minute
    | Second
    | Undefined of string
    with 
        // use module instead? tk
        static member toTimeUnit(str:string) = 
            match str with
            | "y" -> TimeUnit.Year
            | "mo" -> TimeUnit.Year
            | "w" -> TimeUnit.Year
            | "d" -> TimeUnit.Year
            | "h" -> TimeUnit.Year
            | "m" -> TimeUnit.Year
            | "s" -> TimeUnit.Year
            | _ -> TimeUnit.Undefined str 

        static member getDurationSeconds(tu: TimeUnit) = 
            match tu with 
            | Year   -> 365 * 24 * 3600     // Assuming a non-leap year: 365 days
            | Month  -> 30 * 24 * 3600      // Assuming an average month of 30 days
            | Week   -> 7 * 24 * 3600       // 7 days in a week
            | Day    -> 24 * 3600           // 24 hours in a day
            | Hour   -> 3600                // 1 hour = 3600 seconds
            | Minute -> 60                  // 1 minute = 60 seconds
            | Second -> 1                   // 1 second = 1 second
            | Undefined _ -> 0

        static member ticksPerSecond = 10_000_000L // 10 million ticks per second



    [<RequireQualifiedAccess>]
    type ResolvedValue = 
    | Numeric of NumericValue
    | String of string
    | Boolean of bool
    | BadVal of string
    | FixedDate of System.DateTime  // if these are only part of functions, we may not need them here tk
    | DateOffset of int * TimeUnit  // ditto // would a date offset be a resolved value? perhaps yes it is something that can appear in a binary operation like plus, minus
    // need to add lists tk

