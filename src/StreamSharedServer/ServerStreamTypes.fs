namespace Tweega.Shared

// this is largely infrastructure stuff - it appears in shared because it gets used on the PI Server side
// which uses 4.8.   I think we will need to link into separate versions of the dll

module ServerStreamTypes =
    open Tweega.Shared.XFrameworkTypes
    open Tweega.Shared.ClientStreamTypes
    open Tweega.Discovery.Shared.Types
    open Tweega.AkkaRouter.Types
    open Tweega.Utils
    
    open System

    [<RequireQualifiedAccess>]
    type RequestData = 
    | NextPage
    | Teardown

    type FunctionBuildInfo = {
        QName: QualifiedFunctionName;  // will be same as provider if this is the root definition node
        QFNs: list<QualifiedFunctionName>;  // functions referred to in this function tree and where to find them if need be. we don't make use of this at the moment tk makeoption?
        FunctionTree: FunctionTree;  // the function definition
        OutputType: Tweega.Shared.ClientStreamTypes.StreamType;
    }

    [<RequireQualifiedAccess>]
    type ProxyBrokerMsg = 
        | DeploymentResults of RequestID * option<list<FailureReason>>
        // | ProxyRegistrationMsg of ProxyRegistrationMsg
        | RegisterBuilders of list<QualifiedFunctionName * ActorLocation> * option<Subscriber<Result<bool, string>>>  //subscriber is recipe        // | CreateProxyInfrastructure of QualifiedFunctionName * sampleData: 'Data //Result<StreamAPIBuilderGenerator, FailureReason>
        | RouteStreamAPIMessage of QualifiedTag * StreamAPI //need to incoporate qname tk discovery will use identifier to find mainstream actor and apply yhe passed in function to it. it is assumed that the function will then pass on a typed message to the actor
        | BuildFunction of FunctionBuildInfo *  Subscriber<QualifiedTag * Result<MessageHandler<StreamAPI>, FailureReason>> //Result<FunctionBuilderInfo, FailureReason>
        | BuildResult of RequestID * (QualifiedTag * Result<MessageHandler<StreamAPI>, FailureReason>)
        | SetSelfLocation of ActorLocation
        | SetProviderLocation of ActorLocation  
        | SetFunctionBuilderLocation of ActorLocation //  is this the call for successful deployment?
        | TestMsg of string
        // | ParseCommand of string //StreamCommand //callback? 
        // | DeployProviders of alist<QualifiedFunctionName> * Subscriber<option<list<FailureReason>>>

    
    [<RequireQualifiedAccess>]
    type ServiceRequestResult =
    | ProducerResult of ClientRef * Result<list<TagAlias>, FailureReason>   //where remote is data source
    | ConsumerResult of Result<ActorLocation , string> //where remote is data consumer


    [<RequireQualifiedAccess>]
    type ProxyMsg = // specialist builder API
        | Initialise of QualifiedFunctionName * string // from proxyManager to proxy.  param2 is path to Interactor ? tk
        | StreamRequest of BuilderDoc * Subscriber<BuildInfrastructureResult> // BuilderDoc does not look like a recipe doc so perhaps change name tk
        | ServiceRequestResult of ServiceRequestResult
        | TestMsg of string
        | StreamMsg of MessageHandler<StreamAPI>

    type ProxyState = {
        QualifiedFunctionName: QualifiedFunctionName;
        IsInitialised: bool;
        ClientManagerLocation: ActorLocation; //this appears  to double up as ProxyBroker location when proxy does not have a server componment make this an option and add a proxybroker field also? tk
        StreamRequestMap: Map<string, Subscriber<BuildInfrastructureResult>>;
        PiperLocation: ActorLocation;
        Dependencies: list<string * MessageHandler<StreamAPI>>;
        // PipeAPIMap: Map<ServerRef, MessageHandler<StreamMsg<TimeSeriesValue<System.DateTime>,TimeSeriesValue<System.DateTime>,list<TaggedValues<TimeSeriesValue<float>>>, list<TaggedValues<TimeSeriesValue<float>>>,unit>>>
        // StreamSet: Set<TagAlias>;  // list of tags that we have subscribed for -- would we want to double check with the server if not found?
    }
    type NO_STATE = string
    let NO_BUFFER_STATE:NO_STATE = ""
    let NO_STREAM_STATUS_PROPAGATION:SubscriberID * StreamStatus * StreamStatus -> option<SubscriberID * StreamStatus * StreamStatus>= fun(_a,_b,_c) -> None

    type TeardownAction = unit -> unit

    type BatchID = string

    type StreamSourceMap(m: Map<TagAlias,(System.TypeCode * (unit -> unit) * obj)>) =

        let mutable sourceMap: Map<TagAlias,(System.TypeCode * (unit -> unit) * obj)> = m
        new() = StreamSourceMap(Map.empty)

        // member this.addSource<'T>(tagName: TagAlias, subscriptionPoint: Subscriber<Subscriber<'T>>) =
            // does arg2 need to be boxed? subscribeToSource takes generic function arg
            // if it comes in unboxed, we can store data type info with it to help with error reporting if unboxing fails
        member __.addSource(tagName: TagAlias, tc: TypeCode, streamStarter: (unit -> unit), subscriptionPointObj: obj) =
            sourceMap <- sourceMap.Add (tagName, (tc, streamStarter, subscriptionPointObj))

        member __.startSources() =
            sourceMap |>
            Map.iter(fun _key (_tc, starter, _subObj) ->
                starter()
            )
            //do we need confirmation that streams have started? tk
        member __.subscribeToSource(tagAlias: TagAlias, subscriptionInfo: SubscriberID * Subscriber<TaggedValues<TimeSeriesValue<'r>>> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>) =
            toConsole( sprintf "subscribeToSource: %s" tagAlias)
            let tr = typeof<'r>
            let tcStr = tr.ToString()
            let maybeObj = Map.tryFind tagAlias sourceMap
            let res =
                match maybeObj with
                | Some (_typeCode, _streamStarter, subscriptionPointObj) ->
                    // string * TaggedValues<TimeSeriesValue<'p1>> -> unit * SubscriberID * StreamStatus * StreamStatus -> unit
                    let maybeSubscriber = tryUnbox<Subscriber<SubscriberID * Subscriber<TaggedValues<TimeSeriesValue<'r>>> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>>> subscriptionPointObj
                    match maybeSubscriber with
                    | Some subscriberHandler ->
                        subscriptionInfo |> subscriberHandler
                        let msg = sprintf "Successfully subscribed to source of type %s, tag: %s"  tcStr tagAlias
                        Ok msg
                    | None ->
                        let msg = sprintf "Unable to unbox subscriber handler for tag %s, type: %s" tagAlias tcStr
                        toConsole(msg)
                        Error msg
                | None ->
                    let msg = sprintf "Unable to find source for tag %s, type: %s" tagAlias tcStr
                    toConsole(msg)
                    Error msg
            toConsole( "On the way out from subscribeToSource")
            res

    
    type StreamDispose =
        | StreamDispose of (unit -> unit)
        | NotDisposable


    type Initialiser<'state> = unit -> 'state * StreamDispose

    type DispatchStrategy =
        | Dispatch of DispatchWhen
        | NoDispatch

    type Dispatcher<'U> = 'U -> unit
    type Generator<'T, 'U> = 'T -> 'U
    type GenDispatch<'T, 'U> = Generator<'T, 'U> -> Dispatcher<'U> -> Dispatcher<'T>

    // BufferHandler takes an input and two buffer lists,and returns a list of values to process, plus two updated buffer lists
    // we may also want to add state so that buffer can compress on the fly

    type BufferHandler<'bufInA, 'bufState> = bool -> list<'bufInA> * list<'bufInA> * 'bufState -> list<'bufInA> * list<'bufInA> * 'bufInA list * option<TagAlias * StreamStatus * StreamStatus> * 'bufState

    type BufferInputHandler<'bufIn, 'bufInA, 'bufOut, 'bufState> = 
        option<Generator<list<'bufInA>, 'bufOut>> -> option<'bufIn> * 'bufInA list * 'bufInA list * 'bufState -> 'bufInA list * 'bufInA list * 'bufState

    // might a buffer need to dispatch status updates? tk 
    // to client
    [<RequireQualifiedAccess>]
    type BufferMsg<'bufIn,'bufInA, 'bufOut, 'bufState> =
        | SetGenerator of Generator<list<'bufInA>, 'bufOut> * DispatchStrategy
        | SetStrategy of DispatchStrategy  // acts as a tap on flow through the buffer
        | AddToBuffer of list<'bufIn> //check why this has to be a list - might be to do with piper needing to know that inputs are always lists 
        | AddToBufferBulk of list<'bufInA>
        | TeardownBuffer
        | UpdateState of ('bufState -> 'bufState)


    type BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState> = {
        Generator: Generator<list<'bufInA>, 'bufOut>
        // Dispatcher: Subscriber<'bufInA>  //function that forwards buffer data to connsumer (optionally transforming data on the way)
        BufferInputHandler:BufferInputHandler<list<'bufIn>, 'bufInA, 'bufOut, 'bufState>
        DispatchStrategy: DispatchStrategy
        //dispatcher: Subscription<'bufIn> //this will send a BufferMsg to either another buffer or a stream
        Pending: list<'bufInA>    //values taken off from here
        Backlog: list<'bufInA>    //values placed here, taken off and reversed onto pending
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

    type SocketBufferMsg<'T> = BufferMsg<Tweega.Shared.ClientStreamTypes.TSVDelivery<'T>, list<TSVDelivery<'T>>, WSDelivery<'T>, unit>

    //WSSender is a Subscriber<WSDelivery<'T>> - ie a function that accespts WSDelieveries
    // what we want instead is a function that accepts an emitter and joins it up with the right sender
    type FloatConsumerTTSVs = ClientRef -> list<TaggedValues<TimeSeriesValue<float>>> -> unit
    type EFConsumerTTSVs = ClientRef -> list<TaggedValues<TimeSeriesValue<EventFrameRecord>>> -> unit

    type FloatProducerTSV = Subscriber<list<TaggedValues<TimeSeriesValue<float>>> -> unit>
    type EFProducerTSV = Subscriber<list<TaggedValues<TimeSeriesValue<EventFrameRecord>>> -> unit>

    [<RequireQualifiedAccess>]
    type WSJunctionConsumer =
    | Float of FloatConsumerTTSVs   // |Primitive * ? tk this needs reworking a bit
    // | EF of EFConsumerTTSVs
        with
        member this.DataType() =
            match this with
            | Float _f -> FloatT
            // | EF _f -> StreamType.EF

    // [<RequireQualifiedAccess>]
    // type WSJunctionProducer =
    // | Float of ClientRef * obj //FloatProducerTSV
    // | EF of ClientRef * obj //EFProducerTSV
    //     with
    //     member this.DataType() =
    //         match this with
    //         | Float (_cRef, _oProducer) -> StreamType.Primitive PrimitiveType.Float
    //         | EF (_cRef, _oProducer) -> StreamType.EF

    //     member this.Connect(consumerMap: Map<StreamType, WSJunctionConsumer>) =
    //         // the idea of this function is to connect a consumer with a producer and return a result<bool, string>
    //         let st = this.DataType()
    //         let maybeDeliveryConsumer = consumerMap.TryFind(st)

    //         match (this, maybeDeliveryConsumer) with
    //             | (Float (cRef, oProducer), Some (WSJunctionConsumer.Float floatConsumer)) ->
    //                 let maybeProducer = tryUnbox<FloatProducerTSV> oProducer
    //                 match maybeProducer with
    //                 | Some producer ->
    //                     let consumer = (cRef |> floatConsumer)
    //                     consumer |> producer
    //                     Ok true
    //                 | None ->
    //                     let msg = sprintf "Unable to cast producer to FloatProducerTSV in WSJunctionProducer.Connect. cRef:  %s " cRef
    //                     toConsole(msg)
    //                     Error msg


    //             | (EF (cRef, oProducer), Some (WSJunctionConsumer.EF efConsumer)) ->
    //                 let maybeProducer = tryUnbox<EFProducerTSV> oProducer
    //                 match maybeProducer with
    //                 | Some producer ->
    //                     let consumer = (cRef |> efConsumer)
    //                     consumer |> producer
    //                     Ok true
    //                 | None ->
    //                     let msg = sprintf "Unable to cast producer to FloatProducerTSV in WSJunctionProducer.Connect. cRef:  %s " cRef
    //                     toConsole(msg)
    //                     Error msg

    //             | (t, Some (x: WSJunctionConsumer)) ->
    //                 let tConsumer = x.DataType()
    //                 let tEmitter = t.DataType()
    //                 let msg = sprintf "Conflicting types in TSVDeliveryProducer.Connect.  %s : %s" (tEmitter.ToString()) (tConsumer.ToString())
    //                 toConsole(msg)
    //                 Error msg

    //             | (t, None) ->
    //                 let tEmitter = t.DataType()
    //                 let msg = sprintf "No consumer found in TSVDeliveryProducer.Connect for emitter  %s " (tEmitter.ToString())
    //                 toConsole(msg)
    //                 Error msg


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
        | StreamSubscribe of SubscriberID * Subscriber<'streamVal> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>
        | StreamUnsubscribe of SubscriberID
        | StreamStart of DispatchWhen  // callback for acknowledgement? recipe? tk
        | StreamPause
        | StreamTearDown    // this will have to come with a callback to be used in recipe tk
        | StreamSourceStatusChange of SubscriberID * StreamStatus * StreamStatus // we probably need to also have the name of the provider
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
    

    // WriterAPIInternal allows multiple data sources to stream into the same collection point buffer - here a writer
    // this is not that necessary for a file writer, but perhaps useful for a web socket or aggregator of say multiple pipe flows
    [<RequireQualifiedAccess>]
    type WriterAPIInternal<'T> = //More generic than just writers.  Anything that processes a stream input.  internal sounds more private than this is tk
        | StartWriter
        | StopWriter
        | SubscribeWriter of SubscriberID * Subscriber<WriteResult> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus> // is this the same as streamAPI - should they be merged? tk
        | UnsubscribeWriter of SubscriberID
        | TeardownWriter
        | AddDataSource of FunctionName * Subscriber<SubscriberID * Subscriber<list<'T>> * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>>
        | StreamSourceStatusChange of ClientRef * ServerRef * StreamStatus * StreamStatus // do writers need serverRef? tk
        // | TruncateFile

    type CombinedStreamData<'Data> = list<TaggedValues<TimeSeriesValue<'Data>>>

    // original with CombinedStreamData
    // type WriterAPIInternalTSV<'Data> = WriterAPIInternal<CombinedStreamData<'Data>>
    [<RequireQualifiedAccess>]
    type WriterAPIInternalTSV<'Data> = WriterAPIInternal<TaggedValues<TimeSeriesValue<'Data>>> // this is 'BufIn, not 'BufinA
    // type TransformAPIInternalTSV<'Data> = TransformAPIInternal<TaggedValues<TimeSeriesValue<'Data>>> // this is 'BufIn, not 'BufinA


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
                let maybeMsg = Tweega.Utils.tryUnbox<Subscriber<list<TaggedValues<'StreamData>>>> (objSubscriber)
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
    type StreamResultWithDataStore<'Data> = (DataStore * TagAlias * Result<MessageHandler<TypedStreamAPI<StreamData<'Data>>>, list<FailureReason>>)

    type StreamFails = list<FunctionTree * list<FailureReason>>
    type StreamDoc<'Data> =  //this is the format for the function that will route the request to the stream supplier - perhaps supplier does not have to work on doc format  the doc can be assembled later.
        {
            // input
            StreamQueries: list<FunctionTree> // by this stage we should know what our data type is
            // output
            StreamResults: list<StreamResultWithFunctionName<'Data>>
        }

    type StreamSpecs<'Data> = Subscriber<list<StreamResultWithFunctionName<'Data>>> * list<FunctionTree>

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
    type RepoQuery = {
        DataStore: DataStore
        Tag: string
    }
    with
        member this.serialise() : list<KVP>  =
            let ds =
                match this.DataStore with 
                | DataStore.PIAF ->"piaf"
                | DataStore.File -> "file"
                | DataStore.WS -> "web_socket"

            [{key="data_store"; value=ds}; {key="tag"; value=this.Tag}]

        static member deserialise(kvpList: list<KVP>) : RepoQuery =            
            let optionsMap = kvpList |> Tweega.Utils.kvpsToMap

            //can't load tweega.utils for some reason to use Exists active pattern
            let ds = 
                match Map.tryFind "data_store" optionsMap with 
                | Some dStr ->
                    match dStr with 
                    | "piaf" -> DataStore.PIAF
                    | "file" -> DataStore.File
                    | "web_socket"-> DataStore.WS
                    | _ -> DataStore.File  // don't bother with the discriminated union for data stores tk

                | _ -> DataStore.File
            let tag = 
                match Map.tryFind "tag" optionsMap with 
                | Some t -> t
                | None -> "TagRepo" //temporary only tk
            {Tag = tag; DataStore = ds}
    


    // type StreamServerAPI<'T> =
    //     | StreamAPI of TypedStreamAPI<'T>
    //     | StreamAdminAPI of StreamAdminAPI  //API for internal Pipeline management

    //when registering a stream source, i provide a function that takes
        // a callback to return a stream doc
        // a stream doc containing query


    type StreamRequestID = string

    // references to RepoQuery are replay specific?  if so, move. tk
    type RepoRequester<'Data> = Subscriber<list<StreamResultWithDataStore<'Data>>> * list<RepoQuery>
    type StreamRequester<'Data> = Subscriber<list<StreamResultWithFunctionName<'Data>>> * (list<FunctionTree> * option<list<KVP>>)
    type StreamRequester2<'Data> = Subscriber<list<StreamResultWithFunctionName<'Data>>> * (list<RepoQuery> * option<list<KVP>>)
    type StreamBuilder<'Data> = Subscriber<StreamRequester<'Data>>
    //and MainstreamCallback<'Data> = StreamRequestID * list<(TagAlias * Result<MessageHandler<TypedStreamAPI<'Data>>, FailureReason>)>
    //type MainstreamCallback<'Data> = list<StreamSource * StreamResult<'Data>>
    type MainstreamCallback<'Data> = list<MainstreamTAlias * TagAlias * Result<MessageHandler<TypedStreamAPI<CombinedStreamData<'Data>>>,list<FailureReason>>>


    // API for the stream proxy
    [<RequireQualifiedAccess>]
    type MainstreamTMsg<'Data> =
        | Init
        | RegisterStreamSource of FunctionAlias * StreamBuilder<'Data>    //messages for stream source are handled by this
        | UnregisterStreamSource of MainstreamTAlias
        | StreamBuilderResult of StreamRequestID * list<StreamResultWithFunctionName<'Data>>
        | UnregisterStream of TagAlias
        | StreamSourceMsg of StreamID * TypedStreamAPI<'Data>   // entry point for messages for stream source
        | BuildStreams of Subscriber<list<StreamResultWithFunctionName<'Data>>> * (list<FunctionTree> * option<list<KVP>>) //KVPs should be inside the list
        | StreamSubscriptionCancel of StreamRequestID
        | TeardownStream of FunctionAlias
        | TeardownStreams
    
    type TagList = list<string>
    type ActorPath = string

    // API for the server side stream once pipe is in place
    [<RequireQualifiedAccess>]
    type StreamAPIServerCmd =
        | StreamTeardown
        | StreamStart of SubscriberID  // the actor name of the pipe
        | StreamPause


    // this is an API that only the proxy should have access to
    // in which case it will not implement RemoteDeploy tk
    // remote deploy server needs to know what?
    // might one sever deploy more than one type - say PI deploys ints and float sources - so we here specify which we mean
    // possibly when we register a source we can supply a callback with the necessary data type already buillt in


    // this is more like the message that the proxy would get tbd tk
    [<RequireQualifiedAccess>]
    type StreamServerMsg =
        | SubscribeTags of ClientRef * UserCredentials * TagList
        | StreamAPIMsg of SubscriberRef * StreamAPIServerCmd


    [<RequireQualifiedAccess>]
        type ConnectionResult =
            | ConnectionSuccess of ClientRef * ServerRef
            | ConnectionFailure of ClientRef * string

        // BuildStreamAPIs called once Function tree has been unpacked and parameters identified
        [<RequireQualifiedAccess>]
        type StreamProxyMsg<'Data> =
            | BuildStreamAPIs of Subscriber<list<StreamResultWithFunctionName<'Data>>> * TagList // taglist should be options? tk tbd

        [<RequireQualifiedAccess>]
        type StreamProxyMsgInternalObsolete<'Data> =
            | StreamProxyMsg of StreamProxyMsg<'Data>   // flatten BuildStreamAPIs into here
            | HandleConnectionResult of ConnectionResult
            | PostDeployInit of ActorLocation

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
        abstract SetLocation : ActorLocation -> 'T
        abstract GetLocation : unit -> ActorLocation

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

    type PullerClient =
        {
            StreamStatusMap: Map<string, StreamStatus>
            CleanupPipe: unit -> unit // once all data downloaded the pipe mechanism can be cleaned up
            PullRequests:int;
            SourceLocation:ActorLocation;  //location of server pipe
            LiveStreamCount: int;
            RequestData: Generator<ActorLocation * RequestData, Result<option<ActorLocation>,FailureReason>>; //extnd this so that client can request page for particular tag
            MaybeBufferLocation: option<ActorLocation>; //location of this RepoPipeClientState instance is this actually needed tk?
        }
    
    
    type IPullerClient = 
      abstract GetPullerClient : unit -> PullerClient

    
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
    


