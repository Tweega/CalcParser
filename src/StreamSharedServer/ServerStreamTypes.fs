namespace Tweega.Shared

// this is largely infrastructure stuff - it appears in shared because it gets used on the PI Server side
// which uses 4.8.   I think we will need to link into separate versions of the dll

module ServerStreamTypes =
    open Tweega.Shared.Types
    open Tweega.Shared.ClientStreamTypes
    open Tweega.Discovery.Shared.Types
    open Tweega.AkkaRouter.Types
    open Tweega.AkkaRouter.Mailbox
    open Tweega.Shared.Utils
    
    open System

    [<RequireQualifiedAccess>]
    type RequestData = 
    | NextPage
    | Teardown

    type NO_STATE = string
    let NO_BUFFER_STATE: NO_STATE = ""
    let NO_STREAM_STATUS_PROPAGATION:SubscriberID * StreamStatus * StreamStatus -> option<SubscriberID * StreamStatus * StreamStatus>= fun(_a,_b,_c) -> None

    type TeardownAction = unit -> unit

    type BatchID = string

    type TagAlias = string  //all stream data is expected to be tagged?

    
    type StreamDispose =
        | StreamDispose of (unit -> unit)
        | NotDisposable


    type Initialiser<'state> = unit -> 'state * StreamDispose


    type DispatchStrategy =
        | Dispatch of DispatchWhen
        | NoDispatch

    type Dispatcher<'U> = 'U -> unit
//    type Generator<'T, 'U> = 'T -> 'U
    type GenDispatch<'T, 'U> = Generator<'T, 'U> -> Dispatcher<'U> -> Dispatcher<'T>

    // BufferHandler takes an input and two buffer lists,and returns a list of values to process, plus two updated buffer lists
    // we may also want to add state so that buffer can compress on the fly

    //'bufIn will be backlog, bufInA is pending 
    type BufferHandler<'bufIn, 'bufInA, 'bufState> = bool -> list<'bufInA> * list<'bufIn> * 'bufState -> list<'bufInA> * list<'bufInA> * 'bufIn list * option<TagAlias * StreamStatus * StreamStatus> * 'bufState

    type BufferInputHandler<'bufIn, 'bufInA, 'bufOut, 'bufState> = 
        (option<Generator<list<'bufInA>, 'bufOut>> * Subscriber<'bufOut>) -> option<'bufIn> * list<'bufInA> * list<'bufIn> * 'bufState-> list<'bufInA> * list<'bufIn> * 'bufState 
    //  option<Generator<list<'bufInA>, 'bufOut>> -> option<'bufIn> * list<'bufInA> * list<'bufIn> * 'bufState-> list<'bufIn> * list<'bufInA> * 'bufState
    //  option<Generator<list<'bufInA>, 'bufOut>> -> option<'bufIn> * 'bufInA list * 'bufIn list * 'bufState -> 'bufInA list * 'bufIn list * 'bufState

    // might a buffer need to dispatch status updates? tk 
    // to client
    [<RequireQualifiedAccess>]
    type BufferMsg<'bufIn,'bufInA, 'bufOut, 'bufState> =
        | SetGenerator of Generator<list<'bufInA>, 'bufOut> * DispatchStrategy
        | SetStrategy of DispatchStrategy  // acts as a tap on flow through the buffer
        | AddToBuffer of 'bufIn
        | AddToBufferBulk of list<'bufIn>
        | TeardownBuffer
        | UpdateState of ('bufState -> 'bufState)


    type BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState> = {
        Generator: Generator<list<'bufInA>, 'bufOut>
        // Unpacker: Generator<list<'bufIn>, list<'bufInA>>
        Dispatcher: Subscriber<'bufOut>  //function that forwards buffer data to connsumer (optionally transforming data on the way)
        BufferInputHandler:BufferInputHandler<'bufIn, 'bufInA, 'bufOut, 'bufState>
        DispatchStrategy: DispatchStrategy
        //dispatcher: Subscription<'bufIn> //this will send a BufferMsg to either another buffer or a stream
        Pending: list<'bufInA>    //values taken off from here
        Backlog: list<'bufIn>    //values placed here, taken off and unpacked into pending
        Delay: bool
        BufferState: 'bufState
        BufName: string
    }

    type BufferStateOrig<'bufIn, 'bufInA, 'bufOut, 'bufState> = {
        Generator: Generator<list<'bufInA>, 'bufOut>
        // Dispatcher: Subscriber<'bufInA>  //function that forwards buffer data to connsumer (optionally transforming data on the way)
        BufferInputHandler:BufferInputHandler<'bufIn, 'bufInA, 'bufOut, 'bufState>
        DispatchStrategy: DispatchStrategy
        //dispatcher: Subscription<'bufIn> //this will send a BufferMsg to either another buffer or a stream
        Pending: list<'bufInA>    //values taken off from here
        Backlog: list<'bufInA>    //values placed here, taken off and reversed onto pending
        Delay: bool
        BufferState: 'bufState
        DistinguishFromNewBufferState: bool;
    }

    //stripping out a lot of types here - see commit e2f4bb6 for old listing


    type BufBox<'Msg> = | BufBox of ('Msg -> unit) //buffbox that collects data for this tag

    // do we need DelayedVal? tk
    type RTV<'streamVal> =
        | DelayedVal of (unit -> 'streamVal)
        | StreamVal of 'streamVal

    [<RequireQualifiedAccess>]  
    type StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState,'bufState> = {
        StreamName: string
        State: 'streamState   //make this an option tk? meta data for generator to produce next value or to define continuation strategy - obsolete with bufState? tk
        StreamStatus: StreamStatus    //active, paused, stopped
        StreamDisposer: StreamDispose //'unit -> 'unit - we may need to pass state in for clearing out unless termination handler can do this without external instruction tk
        Subscribers: list<Subscriber<'streamVal>>
        SubscriberMap: Map<SubscriberID, Subscriber<'streamVal>>
        StatusSubscriberMap: Map<SubscriberID, Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>>
        Init: Initialiser<'streamState>   //creates initial state and provides a function for releasing any resources created during initialisation
        BroadcastPredicate: 'streamState -> bool  //i don't think that we need this any more tk  it is not used and buffer slicers have more control
        HeadBuffer: Subscriber<BufferMsg<'headBufIn, 'headBufInA, 'headBufOut,  'bufState>> //may become  obsolete with Orchestrator tk
    }

    // 'streamVal implies that the dispatcher may transform data in addition to the generator
    // this is the intention for calculations where more than one buffer is involved.  'bufOut refers to the head buffer
    // while streamVal refers to the value that the emitter finally gets having been passed through n buffers.
    // group these messages into upstream/downstream groups? tk
    [<RequireQualifiedAccess>]
    type StreamMsg<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState,'bufState> = //'bufIn is the type of the head buffer
        | StreamValue of RTV<'streamVal>
        | StreamSubscribe of ClientRef * Subscriber<'streamVal> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>
        | StreamUnsubscribe of ClientRef
        | StreamStart of DispatchWhen  // callback for acknowledgement? recipe? tk
        | StreamPause
        | StreamTearDown    // this will have to come with a callback to be used in recipe tk
        | StreamSourceStatusChange of ClientRef * StreamStatus * StreamStatus // we probably need to also have the name of the provider
        | SetInit of Initialiser<'streamState>    //for when an external source like system.timer will provide data for this stream - when the intialiser needs to initialise a data source and connect it up to the mailbox.  Consider controlling access to this service
        | SetBuffer of Subscriber<BufferMsg<'headBufIn, 'headBufInA, 'headBufOut,'bufState>>
        | UpdateState of ('streamState  -> 'streamState)

    type TimerMsg = StreamMsg<Timers.ElapsedEventArgs,Timers.ElapsedEventArgs,TimeSeriesValue<Timestamp>,list<TimeSeriesValue<Timestamp>>,unit,NO_STATE>
    type TSVStreamMsg<'Data> = StreamMsg<TimeSeriesValue<Timestamp>,TimeSeriesValue<Timestamp>,TaggedValues<TimeSeriesValue<'Data>>,TaggedValues<TimeSeriesValue<'Data>>,unit, NO_STATE>

    //StreamSubscribeTags assumes that the stream is not tagging its own data as it should tk
    // ClientStreamAPIMsg should move to a Pipeline or something representing a client API.
    // it has a similar function to the Replay_API and can be used to stream data to a particular client
    // so each client has its own application instance running tk

    //type BufferedStreamAPI<'T> =
        //a buffered stream can be stopped


    // this is needed by pi writer, so here, but ideally would be in writer types - need to refactor tk - see issue #17

    // [<RequireQualifiedAccess>]
    // type TransformAPIInternal<'T> = // this is essentially the same as streamAPI except that it feeds into a buffer and also allows updates of config
    //     | StartTransform
    //     | StopTransform
    //     | SubscribeTransform of SubscriberID * Subscriber<WriteResult> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>> // is this the same as streamAPI - should they be merged? tk
    //     | UnsubscribeTransform of SubscriberID
    //     | TeardownTransform
    //     | UpdateConfig of Options // may also be useful for writers tk
    //     // add data source allowed for writers to take multiple sources which is harder to see a use case  
    //     // we would have to add this back if merging with WriterAPIInternal
    //     // | AddDataSource of FunctionName * Subscriber<SubscriberID * Subscriber<'T> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>>
    //     | StreamSourceStatusChange of SubscriberID * StreamStatus * StreamStatus
    

    
    type CombinedStreamData<'Data> = list<TaggedValues<TimeSeriesValue<'Data>>>

    
    // StreamSubscribe / Unsubscribe may disappear from this API as subscriptions are done through function trees tk
    // via the mainstreamT
    [<RequireQualifiedAccess>]
    type TypedStreamAPI<'StreamData> = //As in <list<TaggedValues<'StreamData>>>> - but only if using maybeMapFromStreamAPI this does not seem right tk
    | StreamSubscribe of ClientRef * Subscriber<'StreamData> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus> //live stream request
    | StreamUnsubscribe of SubscriberID
    | StreamTeardown
    | StreamStart of DispatchWhen
    | StreamPause
    | StreamSourceStatusChange of ClientRef * ServerRef * StreamStatus * StreamStatus
    | StreamPull
        with  
        static member maybeMapFromStreamAPI =
            // only replayCLient uses this - so maybe nobody - but the unboxing below does not look as if it will work as expected tk 
            fun (streamAPI:StreamAPI)->
            match streamAPI with 
            | StreamAPI.StreamSubscribe (clientRef, objSubscriber, statusUpdater) ->
                // this would be a subscription to the WriteResult of the writer
                let maybeMsg = tryUnbox<Subscriber<list<TaggedValues<'StreamData>>>> (objSubscriber)
                match maybeMsg with 
                | Some subscriber ->
                    toConsole( "Successful cast ppp")
                    Some ((clientRef, subscriber, statusUpdater) |> TypedStreamAPI.StreamSubscribe)
                | None -> 
                    // let maybeMs1 = Tweega.Utils.tryUnbox<Subscriber<TaggedValues<TimeSeriesValue<RAG>>>>(objSubscriber)
                    // let maybeMs2 = Tweega.Utils.tryUnbox<Subscriber<list<list<TaggedValues<TimeSeriesValue<RAG>>>>>>(objSubscriber)
                
                    toConsole( "Error Unable to cast obj subscriber into  Subscriber<list<TaggedValues<TimeSeriesValue<float>>>>>")
                    None
                    
            | StreamAPI.StreamUnsubscribe subscriberID ->
                // None
                Some (subscriberID |> TypedStreamAPI.StreamUnsubscribe)

            | StreamAPI.StreamTeardown ->
                Some TypedStreamAPI.StreamTeardown
                
            | StreamAPI.StreamStart dispatchWhen ->
                Some (TypedStreamAPI.StreamStart dispatchWhen)
                
            | StreamAPI.StreamPause -> 
                Some TypedStreamAPI.StreamPause

            | StreamAPI.StreamPull -> 
                Some TypedStreamAPI.StreamPull


    // debug type tk
    type StreamAPIRouterDebug<'Data> = Subscriber<TypedStreamAPI<TaggedValues<TimeSeriesValue<'Data>>>>

    // type StreamBufIn = TimeSeriesValue<Timestamp>
    // type StreamBufInA = TimeSeriesValue<Timestamp>
    // type StreamBufOut<'Data> = TaggedValues<TimeSeriesValue<'Data>>

    type CombinerBufIn<'Data> = TaggedValues<TimeSeriesValue<'Data>>
    // type CombinerBufInA<'Data> = list<TaggedValues<TimeSeriesValue<'Data>>>
    // type CombinerBufOut<'Data> = CombinerBufInA<'Data>
    // type CombinerBuffer<'Data> = MessageHandler<BufferMsg<CombinerBufIn<'Data>, CombinerBufInA<'Data>, CombinerBufOut<'Data>>>

    type StreamData<'Data> = CombinerBufIn<'Data>
    type StreamResultWithSource<'Data> = (TagAlias * Result<MessageHandler<TypedStreamAPI<StreamData<'Data>>>, list<FailureReason>>)

    //Subscriber<list<TaggedValues<TimeSeriesValue<'Data>>>>
    //The type 'Subscriber<TaggedValues<TimeSeriesValue> list>' does not match the type 'CombinedStreamData<'a>'F# Compiler1

    // type SingleStreamMsg<'Data> = StreamMsg<CombinerBufIn<'Data>, CombinerBufInA<'Data>, CombinerBufOut<'Data>, StreamData<'Data>,unit>
    // type CombinedStreamMsg<'Data> = StreamMsg<CombinerBufIn<'Data>, CombinerBufInA<'Data>, CombinerBufOut<'Data>, CombinedStreamData<'Data>,unit>

    type StreamResult<'Data> = (Result<MessageHandler<TypedStreamAPI<StreamData<'Data>>>, FailureReason>)// was list<FailureReason>
    type StreamResult2<'Data> = (Result<MessageHandler<TypedStreamAPI<TimeSeriesValue<'Data>>>, FailureReason>)
    type StreamResultWithFunctionName<'Data> = (FunctionName * StreamResult<'Data>) // was StreamResult2

    type AdminCallback = TeardownInfo -> unit
    and Dependencies =
        | Deps of list<AdminCallback -> unit>
    and TeardownInfo = Dependencies * list<TeardownAction>

    type BatchRecord =  {
        BatchID: BatchID;
        Tasks: list<AdminCallback -> unit>
    }

    [<RequireQualifiedAccess>]
    type TaskResult =
        | Ok of  BatchID * list<AdminCallback -> unit> //this list of additional tasks that need piping back into the queue
        | Error of BatchID * string

    type TeardownMsg =
        | TeardownRequest of AdminCallback

    // should be in RepoTypes.fs?
    //this should be retured as too inflexible - reeplace with function definition
    [<RequireQualifiedAccess>]
    

    type StreamRequestID = string

    type MainstreamCallback<'Data> = list<MainstreamTAlias * TagAlias * Result<MessageHandler<TypedStreamAPI<CombinedStreamData<'Data>>>,list<FailureReason>>>

    type TagList = list<string>
    type ActorPath = string

    // API for the server side stream once pipe is in place
    [<RequireQualifiedAccess>]
    type StreamAPIServerCmd =
        | StreamTeardown
        | StreamStart of SubscriberID  // the actor name of the pipe
        | StreamPause

    // this is more like the message that the proxy would get tbd tk
    [<RequireQualifiedAccess>]
    type StreamServerMsg =
        | SubscribeTags of ClientRef * UserCredentials * TagList
        | StreamAPIMsg of ClientRef * StreamAPIServerCmd


    [<RequireQualifiedAccess>]
        type ConnectionResult =
            | ConnectionSuccess of ClientRef * ServerRef
            | ConnectionFailure of ClientRef * string

        // BuildStreamAPIs called once Function tree has been unpacked and parameters identified
        [<RequireQualifiedAccess>]
        type StreamProxyMsg<'Data> =
            | BuildStreamAPIs of Subscriber<list<StreamResultWithFunctionName<'Data>>> * TagList // taglist should be options? tk tbd

        // message for client side consumer of stream data - for downstream flows
        [<RequireQualifiedAccess>]
        type PipeConsumerMsg<'In, 'Out> =
            | Payload of list<TaggedValues<'In>> //payload may include data for multiple tags
            | StatusUpdate of ClientRef * ServerRef * StreamStatus * StreamStatus
            | AddStreams of list<TagAlias> // sent by proxyClient when it gets infrastructure results for pipe to build infrastructure
            | StreamAPIMsg of TagAlias * TypedStreamAPI<list<TaggedValues<'Out>>> //for sending message to one of the streams in outgoing pipe ie subscribe to it
            | Test of string
            | TestData of 'In

        
        
            // | StreamAPIMsg of SubscriberRef * TypedStreamAPI<TTSVs<float>>

        [<RequireQualifiedAccess>]
        type ServicePipeAPI<'In> =
            | Payload of 'In
            | StatusUpdate of ClientRef * ServerRef * StreamStatus * StreamStatus
            // | ServiceOutput of MessageHandler<TypedStreamAPI<'Out>>
            | Test of string
            | TestData of 'In


        type StreamAPIInternalTSV<'Data> = TypedStreamAPI<CombinedStreamData<'Data>> // why is this internal? - same as StreamAPITSV tk

        // Proxy messages

        // type PayloadMBState<'Data> = {
        //     Subscribers: list<Subscriber<TTSVs<'Data>>>
        // }

        // [<RequireQualifiedAccess>]
        // type PIAFConnection =
        //     | ConnectionRequest of PIAFCnxInfo * ClientRef
        //     //| ConnectionResponse of ConnectionResult - only needed if setting up connection is async-async - otherwise we can respond to caller immediately
        
        type RAG = 
        | Red
        | Amber
        | Green
        with 
            override this.ToString() = 
                match this with 
                    | Red -> "Red"
                    | Amber -> "Amber"
                    | Green -> "Green"
            

                // this is essentially the beginnings of Orhestrator state
        // RepoCombinerAll plays this role already - all streams are piped through it
        // and -paging  requests sent until all streams report that they are complete.
        // if we follow this model, there will be a similar actor to repoCombinerAll
        // which will be the orchestrator.
        // this won't work if the stream types are different
        // but the orchestrator can still be set up to receive upstream status updates
        // this will get updates from repoCombinerAll- a role that will eventually be taken by the orchestrator
        // so the calc will eventually sign up to the orchestrator for status updates
        // and the orchestrator will get  the status updates from each of the inputs - like the repo combiner is at presents
        type OrchestratedCalcState = {
        TBD: NO_STATE
    }    

    type ILocation<'T> =
        abstract SetLocation : ActorLocation<'T> -> 'T
        abstract GetLocation : unit -> ActorLocation<'T>

    type IAddressResolution<'T> =
        abstract SetAddressResolution : AddressResolution -> 'T
        abstract GetAddressResolution : unit -> AddressResolution

    type IBufferLocation<'T> =
        inherit ILocation<'T> 


    // temporary location until we find best place to share
    type Puller =
        {
            PullDataMap: Map<string, StreamStatus * MessageHandler<RequestData>> //function that requests next page of data from repo inputs via StreamMap on the combiner state
            CleanupInputs: unit -> unit // called when repo combiner slicer finds out that all inputs have terminated
            PullRequests:int;
        }

    type PullerClient<'Msg> =
        {
            StreamStatusMap: Map<string, StreamStatus>
            CleanupPipe: unit -> unit // once all data downloaded the pipe mechanism can be cleaned up
            PullRequests:int;
            SourceLocation:ActorLocation<'Msg>;  //location of server pipe
            LiveStreamCount: int;
            RequestData: Generator<ActorLocation<'Msg> * RequestData, Result<option<ActorLocation<'Msg>>,FailureReason>>; //extnd this so that client can request page for particular tag
            MaybeBufferLocation: option<ActorLocation<'Msg>>; //location of this RepoPipeClientState instance is this actually needed tk?
        }
    
    
    type IPullerClient<'Msg> = 
      abstract GetPullerClient : unit -> PullerClient<'Msg>

    
    type IPuller =
        abstract  GetPuller :unit -> Puller
        
    type IBufferStream<'T> =
        inherit IAddressResolution<'T> 
        abstract GetStatus : unit -> StreamStatus
        abstract SetStatus : StreamStatus -> 'T
        abstract GetName : unit -> string
        
    

    type IStreamConsumer<'T> =
        inherit IBufferStream<'T> 
        // inherit IPuller
        abstract GetInputStatus : ClientRef -> StreamStatus    // a stream can  have more than one input
        abstract SetInputStatus : ClientRef * StreamStatus -> 'T  //string is the nameof the input stream
        abstract GetAllInputsTerminated : unit -> bool // if all inputs are either terminated 
        abstract GetAnyInputsTerminated : unit -> bool // if all inputs are either terminated
        abstract GetAllInputsDataComplete : unit -> bool // if all inputs are either terminated or ready to send data
        abstract GetLivePullCount : unit -> int * list<string>

    // used for  the client end of a pipe paging repo data.  We may not need this to be an interface - check tk
    // perhaps rename to PagedData?


    type BufferStreamState = 
        {
            StreamStatus: StreamStatus;
            // InputStatusMap: Map<ClientRef , StreamStatus>
            BufferAddress: AddressResolution;
            BufferName: string;
            //Orchesrator? and the orchestrator will have  the buffer references?
        }
        
        interface IBufferStream<BufferStreamState> with
            member this.GetName() = this.BufferName;
            member this.GetStatus() = this.StreamStatus
            member this.SetStatus(status) = 
                {this with StreamStatus = status}

            // member this.GetInputStatus(cRef: ClientRef) = 
            //     match this.InputStatusMap with
            //     | Exists cRef streamStatus -> streamStatus
            //     | _ -> StreamStatus.Initialising DateTime.Now // wemust have an unknown stream status

            // member this.SetInputStatus(clientRef, status) = 
            //     let newMap = Map.add clientRef status this.InputStatusMap
            //     {this with InputStatusMap = newMap}

            member this.SetAddressResolution(addRes) = 
                {this with BufferAddress = addRes}
            member this.GetAddressResolution() = 
                this.BufferAddress    

            // member this.GetAllInputsComplete() = 
            //         let pred =
            //             fun (ss: StreamStatus) ->
            //                 match ss with 
            //                 | StreamStatus.Terminated _ | StreamStatus.DataComplete _ -> true
            //                 | _ -> false

            //         let inputs =
            //             this.InputStatusMap |>    
            //             Map.fold(fun acc _key status ->
            //                 status :: acc
            //             )[] 

            //         List.AllOf(pred, inputs)
                    
    type BufferWithInputsState = 
        {
            BufferStreamState: BufferStreamState;
            InputStatusMap: Map<string, StreamStatus>
        }
        static member IsTerminated =
            fun (ss: StreamStatus) ->
                match ss with 
                | StreamStatus.Terminated _ -> true
                | _ -> false

        static member IsTerminatedOrDataComplete =
            fun (ss: StreamStatus) ->
                match ss with 
                | StreamStatus.Terminated _ -> true
                | StreamStatus.Active(_, Some _ , _) -> true //Some indicates datetime when data became ready for dispatchs
                | _ -> false
    
        interface IStreamConsumer<BufferWithInputsState> with
            member this.GetName() = this.BufferStreamState.BufferName;
            member this.GetStatus() = this.BufferStreamState.StreamStatus
            member this.SetStatus(status) = 
                let bss = {this.BufferStreamState with StreamStatus = status}
                {this with BufferStreamState = bss}

            member this.SetAddressResolution(addRes) =
                let bss = {this.BufferStreamState with BufferAddress = addRes}
                {this with BufferStreamState = bss}
 
            member this.GetAddressResolution() = 
                this.BufferStreamState.BufferAddress    

            member this.GetInputStatus(cRef: ClientRef) = 
                match this.InputStatusMap with
                | Exists cRef streamStatus -> streamStatus
                | _ -> StreamStatus.Active (ActiveStream.Initialising, None, DateTime.Now) // we must have an unknown stream status tk

            member this.SetInputStatus(clientRef, status) = 
                let msg =  sprintf "Erreichen wir hier ququ: %s -> %A" clientRef status
                toConsole(msg) 
                toConsole(sprintf "%A" status)
                match this.InputStatusMap with 
                | Exists clientRef _v ->
                    let newMap = Map.add clientRef status this.InputStatusMap
                    {this with InputStatusMap = newMap}
                | _ -> 
                    toConsole( sprintf "Error unable to find key %s in InputStatusMap" clientRef)
                    this

            
            member this.GetLivePullCount() =
                this.InputStatusMap |>
                Map.fold(fun (lCount, pullTags) tag (streamStatus) ->
                    match streamStatus with
                    
                    | StreamStatus.Active (active, maybeComplete, _) -> 
                            
                        let tags =
                            match active with 
                            | ActiveStream.Paused ->
                                pullTags
                            | _ ->
                                match maybeComplete with
                                | Some _dt ->
                                    pullTags
                                | None -> 
                                    tag :: pullTags
                        (lCount + 1, tags)
                    | _-> 
                        (lCount, pullTags) 
                ) (0, [])
                
            member this.GetAllInputsTerminated() =
                // this is used when a consumer wants a single complete delivery of a data set that may have been paged
                // it may be the only use case for the DataComplete status modifier.  Perhaps get rid of this and get  consumer to
                // buffer until terminated status received and then sort. review tk
                
                let inputs =
                    this.InputStatusMap |>    
                    Map.fold(fun acc _key status ->
                        status :: acc
                    )[] 

                List.AllOf(BufferWithInputsState.IsTerminated, inputs)
    
            // a calculation comsumer zipping inputs will notbe able to proceed if any input has stopped sending data
            member this.GetAnyInputsTerminated() = 
                
                let inputs =
                    this.InputStatusMap |>    
                    Map.fold(fun acc _key status ->
                        status :: acc
                    )[] 

                List.AnyOf(BufferWithInputsState.IsTerminated, inputs)
            
            member this.GetAllInputsDataComplete() =
                // this is used when a consumer wants a single complete delivery of a data set that may have been paged
                // it may be the only use case for the DataComplete status modifier.  Perhaps get rid of this and get  consumer to
                // buffer until terminated status received and then sort. review tk
                
                let inputs =
                    this.InputStatusMap |>    
                    Map.fold(fun acc _key status ->
                        status :: acc
                    )[] 

                toConsole(sprintf "Checking for Data completeness\n %A" inputs)
                List.AllOf(BufferWithInputsState.IsTerminatedOrDataComplete, inputs)
    


