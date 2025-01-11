namespace CalcEngine
open Tweega.Shared
open Tweega.Shared.Types
open Tweega.Shared.ClientStreamTypes
open Parser.ParserTypes
open Tweega.Discovery.Shared.Types
open Tweega.AkkaRouter.Mailbox
open Tweega.Shared.Utils

type OrchestratorBuffer = 
    {GG: int}

[<RequireQualifiedAccessAttribute>]
module OrchestratorBuffer =
    // buffer processing functionality
    let handleBufferInput() = 2 
    let tearDownBuffer() = ()


module Orchestrator =
    type OrchestratorBufferState = {
        LastTimeStamp: System.DateTime;

    }

    type OrchestratorAPI =
        | NewValues of TaggedValues<TimeSeriesValue<ResolvedValue>>
        | StreamStatusUpdate of StreamID * StreamStatus

    type OrchestratorState = {
        Buffers: Map<StreamID, list<TimeSeriesValue<ResolvedValue>>>
        StreamStatuses: Map<StreamID, StreamStatus>
    }


    // orchestrator subscribes to all inputs which are mapped to RVs on the way in
    // all values are added to respective buffers
    // then each buffer is asked for time range
    // the orchestrator determines the extent to which the time ranges overlap
    // gets a list of all times within that range
    // then for each XTime, asks buffers for a value for that time
    // then tells buffers to delete entries that are OLDER than that time
    // when we have processed that list, we dispatch to execution actor
    // which takes in a list of unit -> unit or possibly unit -> WriteResult
    // so the executer is a writer and we want to be able to get the WriteResult back to the Orchestrator
    // which would imply that the orchestrator has an output buffer which can be turned off
    // what if I have a sequence of calculations? which is how AF Analyses are structured?
    // then I need to be able to compose the workflow into a single function
    // which should be possible - I need to know how the whole tree hangs together
    // note that each calculation will have one or more outputs where data needs to be written somewhere
    // so the function needs to accumulate KVP of (tagName, timestamped RV) and these need to be placed on an additional stack
    // as the calculation is evaluated.
    // where an input to a calculation is an output of earlier stage then we 
    // modify the binary operator tree accordingly so that we have a single expression
    // parse each expression which should give us a root bin op
    // then for expressions that consume one of the earlier stages, do we want to execute it again
    // or 
    // we delete all times that are older than the minimum of the minimums
    // the orchestrator keeps time range for each tag - or the buffer can answer this question
    // if all tags have a time range available,
    // the orchestrator selects the minimum time from available
    // if all can supply a value for that time


    open System

    // type ResolvedValue =
    //     | Numeric of NumericValue


    // Generic subscriber creation for data coming into the orchestrator
    let createConverter<'T>
        (ctor: 'T -> ResolvedValue) =
        fun (tvs: list<TaggedValues<TimeSeriesValue<'T>>>) ->
            tvs
            |> List.fold (fun acc { Tag = tag; Values = vals } ->
                let rvValues =
                    vals
                    |> List.map (fun { Timestamp = ts; Value = value } ->
                        let rv = value |> ctor
                        { Timestamp = ts; Value = rv }
                    )
                { Tag = tag; Values = rvValues } :: acc
            ) []

    let orchestratorUnpacker() = 
        // this receives a list<TaggedValues<TimeSeriesValue<'T>>>
        // it extracts the timestamp and adds to its own unique list of execution times
        // it also extracts the tag name, and fetches the asscociated tag buffer
        // and sends the new value on to that - at the moment these buffers are not actors
        1

    let orchestratorBufferUnpacker() = 
        1

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

        let orchestratorSlicer = 
            fun 
                (bSend: bool) 
                (pending:list<TaggedValues<TimeSeriesValue<ResolvedValue>>>, 
                    backlog: list<TaggedValues<TimeSeriesValue<ResolvedValue>>>, 
                    state: OrchestratorBufferState) ->
                
                let (pending', tsvs:list<TimeSeriesValue<ResolvedValue>>) = 
                    match pending with 
                    | [] -> ([], [])  // nothing in pending
                    | h :: t -> (t, h.Values)

                // fold over the backlog - assumption seems to be that the backlog will always be emptied after this
                let newTSVs, newPending, newActiveState= 
                    backlog |>
                    List.fold(fun (accTSVs,accPending, accState) (ttsv:TaggedValues<TimeSeriesValue<float>>) ->
                        // fold over the tsvs

                        ttsv.Values |>  
                        List.fold(fun (accTSVs', accPending', accState') (tsv: TimeSeriesValue<float>) ->
                            match state.IncludePredicate(tsv.Value) with
                            | true -> 
                                // reportable -- add to collection
                                let (startTicks: option<int64>, endTicks:option<int64>) = 
                                    match accState'.MaybeStartTicks with 
                                    | None -> (Some tsv.Timestamp.Ticks, None)
                                    | _tix -> 
                                        match accState'.MaybeEndTicks with 
                                            | None -> accState'.MaybeStartTicks, (Some tsv.Timestamp.Ticks)
                                            | _ -> accState'.MaybeStartTicks,accState'.MaybeEndTicks
                                            
                                let newState = 
                                    {accState' with MaybeStartTicks = startTicks; MaybeEndTicks = endTicks}
                                tsv :: accTSVs', accPending', newState
                            | false ->  
                                // not reportable - start new collection ttsv and add this collection set to  pending
                                let newState = {accState' with MaybeStartTicks = None; MaybeEndTicks = None}
                                match accTSVs' with
                                | [] -> accTSVs', accPending', newState
                                | _ ->
                                    [], {Tag = tagName; Values = accTSVs'} :: pending', newState
                        ) (accTSVs, accPending, accState)
                    ) (tsvs, pending', bufState)
                //([], pending, backlog, bufState)

                
                // newTSVs is still active and will be the new Pending if bSend is true
                let dispatch, finalPending = 
                    match (state.DurationPredicate (state.MaybeStartTicks, state.MaybeEndTicks, bSend)) with
                    | true ->
                        match newTSVs with 
                            | [] -> newPending, []
                            | _ -> 
                                newPending, [{Tag = tagName; Values = newTSVs}]
                    | false ->
                        [], {Tag = tagName; Values = tsvs} :: newPending


                // we have processed all the tsvs in backlog
                // if pipe is off keep results in pending, otherwise 
                toConsole( sprintf "Dispatch count: %d" dispatch.Length)
                dispatch, finalPending, [], None, newActiveState    //ActiveBand does bot report state changes(None)

        let unpackBufInA = makeUnpacker id
        createSlicedStreamAPI tagName activeBandSlicer unpackBufInA id bufState
        


        
        let continuation(rvs:list<TaggedValues<TimeSeriesValue<ResolvedValue>>>) = 
            // dispatch rvs to the orchestrator
            // WORKING HERE
            // the orchestrator does not exist yet
            // and we need that before we can subscribe to data sources
            // it may be that we will need to send an init message to the orchestrator
            // and here just put data sources into state
            ()

        let streamStatusUpdateHandler(clientRef, serverRef, prevStreamStatus, newStreamStatus) = 
            ()

        // for each tag we want to be able to unsubscribe from the data source
            // should subscriptions be done as part of a recipe?  We want to know the status of each subscription attempt
            // if so we would need to continue via some message handler in the orchestrator which we have not yet created
            // and for that we might need an init message to actually do this, as the data source may be remote

        let subscribeToDataSources() =
            subscriptionPoints |> 
            List.iter(fun (_tag, dt, streamSource) ->
                    match dt with 
                    | DataType.Numeric num ->
                        match num with 
                        | Number.Float64 ->
                            // create a subscriber that will wrap floats into RVs
                            // we can probbaly abstract away more of this function.
                            let converter = createConverter<float> (NumericValue.Float64 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource

                        | Number.Float32 ->
                            let converter = createConverter<float32> (NumericValue.Float32 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int64 ->
                            let converter = createConverter<int64> (NumericValue.Int64 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int32 ->
                            let converter = createConverter<int32> (NumericValue.Int32 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int16 ->
                            let converter = createConverter<int16> (NumericValue.Int16 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource
                        | Number.Int8 ->
                            let converter = createConverter<int8> (NumericValue.Int8 >> ResolvedValue.Numeric)
                            let boxedSubscriber = (converter >> continuation) |> box
                            // now subscribe to this stream API 
                            let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                            subscribeMsg |> streamSource

                    | DataType.String ->
                        let converter = createConverter<string> ResolvedValue.String
                        let boxedSubscriber = (converter >> continuation) |> box
                        // now subscribe to this stream API 
                        let subscribeMsg:StreamAPI = StreamAPI.StreamSubscribe ("clientRef", boxedSubscriber, streamStatusUpdateHandler)
                        subscribeMsg |> streamSource

                    | DataType.Boolean ->
                        let converter = createConverter<bool> ResolvedValue.Boolean
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
    


    