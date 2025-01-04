namespace Tweega.Discovery.Shared

module Types =
    open Tweega.Shared.Types
    open Tweega.Shared.ClientStreamTypes
    open System

    type RequestID = string
    type DataTypeObsolete = string  // have to put some thought into how to represent data types safely - class GUIDs?
    type DeploymentID = string
    type StreamAPIobj = obj

    type TagAlias = string  //include UOM with tag? as in a named supplier of these units tk
    type Timestamp = System.DateTime

    type TimeSeriesValue<'T> = {
        Timestamp: Timestamp
        Value: 'T
    }

    type TaggedValues<'T> = {
        Tag: string
        Values: list<'T>
    }

    // type BuilderSignature = string
    type BuilderAlias = string
    type StreamTypeAlias = string


    [<RequireQualifiedAccess>]
    type StreamType2 = string    // this will map to a guid

    [<RequireQualifiedAccess>]
    type InputApplicationOutput = // the result of a applying inputs to afunction tree will be one of 
        | StreamAPI of Subscriber<obj> // a StreamAPI wrapped in a function that takes an object,  casts and delivers it to the right actor that the builder has created
        // | FunctionTree of FunctionInfo * Subscriber<FunctionTree * Subscriber<BuildInfrastructureResult>> // another function tree
        // | DynamicFunctionTree of FunctionInfo  * Subscriber<FunctionTree * Subscriber<BuildInfrastructureResult>> * option<Subscriber<obj>> // both the above

    
    type TSVsStatus<'Data> = {
        StreamStatus: StreamStatus;
        TSVs: list<'Data>
    }
    
    type TaggedValuesStream<'Data> = TaggedValues<TSVsStatus<'Data>>
    
    type TaggedValuesStreamOrig<'T> = {
        StreamStatus: StreamStatus;
        TaggedValues: TaggedValues<'T>
    }

    type DispatchWhen =
        | AlwaysDispatch
        | SingleDispatch


    type BoxedSubscriptionPoint = obj

    [<RequireQualifiedAccess>]
    type StreamAPI = //  the same as TypedStreamAPI but for the function manager to make request without knowing type so it can process lists of inputs of different types
    | StreamSubscribe of ClientRef * BoxedSubscriptionPoint * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus> //live stream request
    | StreamUnsubscribe of ClientRef
    | StreamTeardown    //who should be allowed to do this? tk REMOVE this tk it is not a client side thing.
    | StreamStart of DispatchWhen
    | StreamPause
    | StreamPull

    type ResolvedOptionsParameter = 
        | ResolvedOptionsParameter of Options
            

    type Generator2<'T, 'U> = 'T -> 'U // this is duplicated in serverstreamtypes tk

    