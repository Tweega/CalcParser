namespace CalcEngine
open Tweega.Shared
open Tweega.Shared.Types
open Tweega.Shared.ClientStreamTypes
open Parser.ParserTypes
open Tweega.Discovery.Shared.Types
open Tweega.AkkaRouter.Mailbox
open Tweega.Shared.Utils
module Orchestrator =

    type OrchestratorAPI =
        | NewValues of TaggedValues<TimeSeriesValue<ResolvedValue>>
        | StreamStatusUpdate of StreamID * StreamStatus

    type OrchestratorState = {
        Buffers: Map<StreamID, list<TimeSeriesValue<ResolvedValue>>>
        StreamStatuses: Map<StreamID, StreamStatus>
    }

    open System

    // type ResolvedValue =
    //     | Numeric of NumericValue

    // Explicit conversion function
    let convertValue<'T, 'U when 'T :> IConvertible and 'U :> IConvertible> (value: 'T) : 'U =
        match box value with
        | :? 'U as u -> u // Fast path: value is already of the desired type
        | _ -> Convert.ChangeType(value, typeof<'U>) :?> 'U

    // Generic subscriber creation
    let createConverter<'T, 'U when 'T :> IConvertible and 'U :> IConvertible>
        (ctor: 'U -> ResolvedValue) =
        fun (tvs: list<TaggedValues<TimeSeriesValue<'T>>>) ->
            tvs
            |> List.fold (fun acc { Tag = tag; Values = vals } ->
                let rvValues =
                    vals
                    |> List.map (fun { Timestamp = ts; Value = value } ->
                        let convertedValue = convertValue<'T, 'U> value
                        let rv = convertedValue |> ctor
                        { Timestamp = ts; Value = rv }
                    )
                { Tag = tag; Values = rvValues } :: acc
            ) []

    
    let createStreamSourceBuffer(subscriptionPoints: list<TagAlias * Parser.ParserTypes.DataType * Subscriber<StreamAPI>>) = 
        // stream source will supply ResolvedValues as lists containing single ResolvedValues
        // higher upstream we will need a buffer that takes in tsvs<float> etc
        // and spit them out as resolved values

        (*
        We want to be able to subscribe to TaggedValue<float> or TaggedValue<int> etc        
        subscribing functions (which are boxed) accept a boxed subscriber
        along with the subscribing functions we will get type information
        We get passed in StreamAPI handler which accepts a boxed subscriber
        We then create a function that takes such a boxed subscription and casts it to the correct type

        union StreamAPI =
  | StreamSubscribe of SubscriberID * obj * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>

        let mapMsg = fun (streamAPI: StreamAPI) ->
                
                    let maybeMappedMsg: option<TypedStreamAPI<list<TaggedValues<TimeSeriesValue<float>>>>> = 
                        match streamAPI with 
                        | StreamAPI.StreamSubscribe (clientRef, objSubscriber, statusUpdater) ->
                            // this would be a subscription to the output of the calculation
                            let maybeMsg = Tweega.Utils.tryUnbox<Subscriber<list<TaggedValues<TimeSeriesValue<float>>>>>(objSubscriber)
                            match maybeMsg with 
                            | Some subscriber ->
                                //console.log toConsole( "Successful cast"
                                Some ((clientRef, subscriber, statusUpdater) |> TypedStreamAPI.StreamSubscribe)
                            | None -> 
                                // let maybeMs1 = Tweega.Utils.tryUnbox<Subscriber<TaggedValues<TimeSeriesValue<float>>>>(objSubscriber)
                                // let maybeMs2 = Tweega.Utils.tryUnbox<Subscriber<list<TaggedValues<TimeSeriesValue<float>>>>>(objSubscriber)
                            
                                toConsole( "Error Unable to cast obj subscriber into  Subscriber<list<TaggedValues<TimeSeriesValue<float>>>>>")
                                None
                
        *)
        
        let continuation(rvs:list<TaggedValues<TimeSeriesValue<ResolvedValue>>>) = 
            // dispatch rvs to the orchestrator
            ()

        let streamStatusUpdateHandler(clientRef, serverRef, prevStreamStatus, newStreamStatus) = 
            ()

        // for each tag we want to be able to unsubscribe from the data source
            // should subscriptions be done as part of a recipe?  We want to know the status of each subscription attempt
            // if so we would need to continue via some message handler in the orchestrator which we have not yet created
            // and for that we might need an init message to actually do this, as the data source may be remote

        let gg() =
            subscriptionPoints |> 
            List.iter(fun (_tag, dt, streamSource) ->
                    match dt with 
                    | DataType.Numeric num ->
                        match num with 
                        | Number.Float64 ->
                            // create a subscriber that will wrap floats into RVs
                            // we can probbaly abstract away more of this function.
                            let converter = createConverter<float, float> (NumericValue.Float64 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource

                        | Number.Float32 ->
                            let converter = createConverter<float32, float32> (NumericValue.Float32 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int64 ->
                            let converter = createConverter<int64, int64> (NumericValue.Int64 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int32 ->
                            let converter = createConverter<int32, int32> (NumericValue.Int32 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int16 ->
                            let converter = createConverter<int16, int16> (NumericValue.Int16 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int8 ->
                            let converter = createConverter<int8, int8> (NumericValue.Int8 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource

                    | DataType.String ->
                        let converter = createConverter<string, string> ResolvedValue.String
                        let boxedSubscriber = (converter >> continuation) |> box
                        // now subscribe to this stream API 
                        let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                        subscribeMsg |> streamSource

                    | DataType.Boolean ->
                        let converter = createConverter<bool, bool> ResolvedValue.Boolean
                        let boxedSubscriber = (converter >> continuation) |> box
                        // now subscribe to this stream API 
                        let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                        subscribeMsg |> streamSource

                    | x -> 
                        toConsole(sprintf "Invalid data type for stream subscription: %s" (x.ToString()))
                    
            )

        let gen: Generator<list<TimeSeriesValue<ResolvedValue>>,TimeSeriesValue<ResolvedValue>>  = // generator extracts head from a list of tokens - only expecting one and vanillaSingletonSlicer enforces that
            fun (rvs:list<TimeSeriesValue<ResolvedValue>>) ->
                match rvs with
                | h :: [] -> h
                | _ -> 
                    let msg = sprintf "error - In createStreamSourceBuffer expecting a list of one RV, but got %d" rvs.Length
                    let msg' = sprintf "%s - received:%A" msg rvs
                    toConsole (msg)
                    {Timestamp =(System.DateTime.Now); Value = ResolvedValue.BadVal msg'}
                    
        let statusHandler =
            fun ((tag, prevStatus, newStatus) as msg) ->
                toConsole( sprintf "Status update received for %s: %A;%A" tag prevStatus newStatus)

        let tempDispatcher = fun _token -> ()  // tokens will be sent to ourself, but we don't know our own address yet, so willl be set in Init
        
        let tokeniser =
            fun(backlog: list<string>) ->
                backlog |>
                List.fold(fun acc s ->  // vanilla slicer will already have reversed the backlog
                    s |> tokenise |> List.rev // the; cat; sat; on; the; mat;
                    |> List.fold(fun acc' token ->
                        token :: acc'
                    ) acc
                ) []  // folding because we may have several strings ["the cat sat on the mat"; "how do you do"]

        let buffHandler = vanillaSingletonSlicer tokeniser

        let parserBufState =
            initialiseBufferState(
                buffName,
                buffHandler, 
                tempDispatcher, 
                NoDispatch, 
                statusHandler, 
                gen, 
                NODELAY, 
                // vanillaUnpack, 
                NO_BUFFER_STATE)

        let handler = 
            fun
                (_mailbox)
                (msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) 
                (bufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) ->
                handleBufferMessage msg bufferState

        1

    let handleMsg msg state =
        match msg with
        | NewValues taggedTSVs ->
            // Update buffer for the stream
            let updatedBuffers = 
                state.Buffers |> Map.add taggedTSVs.Tag taggedTSVs.Values
            // Perform calculations or further processing
            printfn "Received values for %s" taggedTSVs.Tag
            { state with Buffers = updatedBuffers }, Cmd.none
        
        | StreamStatusUpdate (streamId, status) ->
            // Update stream status
            let updatedStatuses = 
                state.StreamStatuses |> Map.add streamId status
            { state with StreamStatuses = updatedStatuses }, Cmd.none


    let createOrchestrator (name: string) 
        (streamSources: (StreamID * (StreamAPI -> unit)) list) =
    
        let initialState = { Buffers = Map.empty; StreamStatuses = Map.empty }
        // create a buffer for each stream source
        let orchestratorLocation = 
            createAkkaMailboxInDefaultSystem<OrchestratorAPI, OrchestratorState> 
                (name, handleMsg, initialState)
        
        // Set up subscriptions for all stream sources
        streamSources 
        |> List.iter (fun (streamId, streamSource) ->
            let callback (taggedTSVs: TaggedValues<TimeSeriesValue<ResolvedValue>>) =
                actorRef.Tell (NewValues taggedTSVs)
            streamSource (StreamAPI.Subscribe (streamId, callback))
        )

        actorRef

        1
    


    