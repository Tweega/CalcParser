namespace Tweega
module StreamInfrastructure =
    open System
    open Tweega.Shared.XFrameworkTypes
    open Tweega.Shared.ClientStreamTypes
    open Tweega.Shared.ServerStreamTypes
    open Tweega.Discovery.Shared.Types
    open Tweega.Utils
    open Akka.FSharp
    open Akka.Actor

    type SinusoidConfig = {
        Name: string
        CyclesPerDay: int
        Amplitude: int
        //Phase: int
    }

    type TimerConfig = {
        Name: string
        TickMilliseconds: int
    }

    type RandomConfig = {
        Name: string
        MinVal: float
        MaxVal: float
        MaxPercentChange: int
        InitialValue: int
    }

   
    let makeUnpacker(unpack:'bufIn -> 'bufInA) =
        fun(bufIn, bufInAs, buffState) ->
            let bufInA = unpack(bufIn)
            (bufInA :: bufInAs, buffState)

    let unpackTaggedValues =  
        fun({Tag=_tag; Values = values}) ->
            values 


    let ignoreStatusUpdates:Subscriber<TagAlias * StreamStatus * StreamStatus> = 
        fun (tag, _, newStatus) -> 
        toConsole( sprintf "Ignoring status update for %s : %A " tag newStatus)

    let ignoreStatusUpdatesTemporarily = ignoreStatusUpdates

    //how will this  work in the event of the emitter falling over?
    let statusUpdatesToEmitter(emitter) =
        fun ((tag, prevStatus, newStatus) as msg) ->
            toConsole( "coco 1")
            (tag, prevStatus, newStatus) |> (StreamMsg.StreamSourceStatusChange >> emitter)

    let vanillaSlicer<'Data, 'bufState> : BufferHandler<'Data, 'bufState> = 
        fun(bSend: bool)(pending:list<'Data>, backlog: list<'Data>, state: 'bufState) ->
        //dispatches whole of backlog if tap open otherwise keeps in backlog  ignores pending
        // data emitted so that oldest is at the head
        // vanilla slicer ignores stream state updates (None)
        match bSend with
        | true ->
            let dispatch = backlog |> List.rev
            dispatch, [], [], None, state

        | false -> 
            [], pending, backlog, None, state

    let calcSlicerPredicate (backlog:list<'Data>, state:BufferWithInputsState) : bool * option<StreamStatus * StreamStatus>=
        //this is an instance of the function referred to to predicate in CalcSlicer
        //this is the slicer for a single input stream toa function
        
        // toConsole( "In calcSlicerPredicate ddd")
        let isc = state :> IStreamConsumer<BufferWithInputsState>
        
        match state.BufferStreamState.StreamStatus with 
        | StreamStatus.Terminated _ ->
            toConsole( "calc stream state is already terminating rmds")
            // allow dispatch if there is still data in the buffer
            (backlog.Length > 0, None)

        | _ ->

            // toConsole( sprintf "PullMappy: %A\n" state.InputStatusMap)
            
            let liveStreamCount, _pullTags = isc.GetLivePullCount() //this will always be positive so irrelevant?
            // or we should stick to it being terminated and so 0 and this calc slicer will  be specialist for 
            // when we have all data downloaded before processing, in which case we are only interested in whether we 
            // still  have data
            // in reality when one input dries up we should terminate after the next calc execution
            // if livecount is 0 then we won't get here at all and the parentstatus should post that downstream
            let currentStatus = state.BufferStreamState.StreamStatus
            let now = System.DateTime.Now
            match state.BufferStreamState.StreamStatus with  //this duplicates the match above tk
            | StreamStatus.Active (ActiveStream.Paused, _, _) ->
                toConsole( sprintf "calcSlicerPredicate status unalterable: %A" state.BufferStreamState.StreamStatus)
                (false, None)
            | StreamStatus.Terminated _ ->  
                toConsole( sprintf "Error: calcSlicerPredicate status terminated: %A" state.BufferStreamState.StreamStatus)
                // this would be unexpected and necessiate teardown
                (false, None)
            | other ->
                let  ts' = 
                    match other with
                    | StreamStatus.Active (_, _, ts) ->  ts
                    | StreamStatus.Terminated ts -> ts
                    
                let bDispatch, streamStatus =
                    match 0  with 
                    | Eq 0 -> 
                        // toConsole( "Is live stream count ever 0 huh?")
                        match backlog.Length > 1 with   //if 1 then this will be removed and dispatched
                        | true ->
                            // toConsole(sprintf  "calcSlicerPredicate Stream status changing to data complete: liveStreamCount: %d" liveStreamCount)
                            (true, currentStatus)

                        | false -> 
                            // we don't get called if the backlog + pending count is 0, so cleanup needs to stem from this terminated state
                            // we never get here and we need to 
                            toConsole( sprintf "never get here? calcSlicerPredicate Stream status changing to TERMINATED: liveStreamCount: %d" liveStreamCount)
                            (true, StreamStatus.Terminated now) //true because we want to dispatch the last data item

                    | _ -> 
                        // toConsole( "calcSlicerPredicate Sttream status ACTIVE for repo combiner")
                        ((backlog.Length > 0), StreamStatus.Active (ActiveStream.Streaming, None, ts'))

                let maybeStatusChange = 
                    match streamStatus.maybeChanged(state.BufferStreamState.StreamStatus) with
                    | Some newStatus ->
                        toConsole( sprintf "calcSlicerPredicate Status CHANgED hyg: %A" newStatus)
                        Some (state.BufferStreamState.StreamStatus, newStatus)
                    | _ -> 
                        // toConsole( "calcSlicerPredicate No change in status gtf")
                        None


                (bDispatch, maybeStatusChange)
            
    
    
    let CalcSlicer 
        (tag:TagAlias)
        (predicate)
        (bSend: bool)
        (
            pending:list<'Data>, 
            backlog: list<'Data>, 
            state: BufferWithInputsState
        ) =
        // this slicer only copes with a single delivery of bufInAs which it stores in backlog 
        // and hands out  one at a time.  To cope with continuous deliveries we need  to  be  able
        // to place data into  backlog as collection then feed into pending
        // pending in this case will probably have to be of a different data type to backlog tk
        // so backlog would be bufIn and would be unpacked into pending (bufInA)
        
        let (bDispatch, maybeNewStreamStatus) = predicate(backlog, state) //predicate is eg calcSlicerPredicate
        // toConsole( sprintf "we have some data in CalcSlicer, bDispatch:%b, bSend: %b" bDispatch bSend)

        let nextState, maybeStatus =
            // record any state changes on the buffer
            match maybeNewStreamStatus  with
            | Some (oldStatus, newStatus) -> 
                let newBufState = 
                    // using the interface for convenience
                    match tryUnbox<IStreamConsumer<BufferWithInputsState>>(state) with
                    | Some pbs -> 
                        toConsole( "Miles Davis setting  status")
                        pbs.SetStatus(newStatus)
                    | _ -> 
                        toConsole(sprintf  "Error: Unable to cast buffer to IStreamConsumer for buffer %s" tag)
                        state
                newBufState, Some(tag, oldStatus, newStatus)
            | None -> state, None

        match bDispatch && bSend with
        | true ->
            let dispatch, backlog' =
                match backlog with
                | h :: t -> [h], t
                | _ -> 
                    // this would imply that repo has no data - how do we get that info to the consumer? tk
                    toConsole( "It would seem that the repo has no data - We need a stream status for that which we would create here and send as the dispatch" )
                    [], []
            
            dispatch, pending, backlog', maybeStatus, nextState

        | false -> 
            [], pending, backlog, None, nextState




    let vanillaCalcSlicer:BufferHandler<'Data, 'bufState> = 
        fun(bDispatch: bool)(pending:list<'Data>, backlog: list<'Data>, bufState: 'bufState) ->
            //ignoring possibility of stream state changes (we are not expecting TsvsStatus data)
            // unpacker puts everything onto backlog so that newest is at head
            // if pending is empty we reverse backlog and put into pending so that oldest is served first

            // calculator like any other stream needs to know when it is finished
            // particularly if feeding a writer

            match bDispatch with
            | true ->
                match pending with
                | [] ->
                    match List.rev backlog  with
                    | [] -> 
                        toConsole( "Warning? No data in either pending or backlog. Is that OK?")
                        [], [], [], None, bufState
                    | h :: t ->
                        [h], t, [], None, bufState  //dispatching data one item at a time

                | h :: t ->
                    [h], t, backlog, None, bufState

            | false ->
                match pending with
                | [] ->
                    match List.rev backlog  with
                    | [] -> 
                        toConsole( "Warning? No data in either pending or backlog. Is that OK?")
                        [], [], [], None, bufState
                    | pending' ->
                        [], pending', [], None, bufState  //dispatching data one item at a time

                | pending' ->
                    [], pending', backlog, None, bufState

    let NODELAY = false
    let DELAY = true

    let inline delayed f a = fun () -> f(a)

    let timerGen(_timerCfg) =
        //you can subscribe to a timer but the timer is not a subscriber
        //create a timer that ultimately emits
        //who starts the timer and who calls this fun here?
        fun (args: list<Timers.ElapsedEventArgs>) ->
            let now = DateTime.Now
            let signalTime = 
                match args with
                | h :: _ -> h.SignalTime
                | _ -> now
            let signalTime = signalTime
            {Timestamp = now; Value = signalTime}

    let timerInit({TickMilliseconds = tickMilliseconds;}, bufBox: MessageHandler<BufferMsg<Timers.ElapsedEventArgs,Timers.ElapsedEventArgs, TimeSeriesValue<Timestamp>,NO_STATE>>) =
        //initialiser is unit -> ('c * (unit -> unit))
        //create the timer? return state and a way to dispose of timer resources
        let subscriptionCallback = ignore   //how to get access to this.  place in state?
        let routeTimerArgToMailbox =
            //this function won't ever be updated
            //SHould it not be passed in?  Continuations for buffers will be passed in but come back to this one tk
            //for streams that do not have to evaluate anything, such as timer, have another class of mailbox tk
            fun(t: Timers.ElapsedEventArgs) ->
                let bufferMsg: BufferMsg<Timers.ElapsedEventArgs,Timers.ElapsedEventArgs, TimeSeriesValue<Timestamp>, NO_STATE> =
                    BufferMsg.AddToBuffer [t]
                bufBox bufferMsg

        fun() ->
            let timer = new Timers.Timer(float tickMilliseconds)
            timer.AutoReset <- true
            timer.Start()

            let disposable =
                timer.Elapsed
                |> Observable.subscribe  routeTimerArgToMailbox

            let disposer = fun() ->
                disposable.Dispose()    //stop the event stream
                timer.Stop()
                timer.Dispose()
                toConsole( "Timer stopped")

            ((), StreamDispose disposer) //unit here indicates that a timer emitter keeps no state (other than disposer)



    let randomFloatInit(cfg: RandomConfig) =
        fun() ->
            ((cfg.MinVal + cfg.MaxVal) / 2.0, NotDisposable)



    let inline staticValueGen(_cfg) =
        fun (state: 'a) (_value: 'b) ->
            ({Timestamp = DateTime.Now; Value = state}, state)

    let maybeBroadcast(v: 'streamVal, streamState: StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>) =
        if streamState.BroadcastPredicate(streamState.State) then
            List.iter(fun subscriber -> subscriber v) streamState.Subscribers

    let disposeOf(sd: StreamDispose) =
        match sd with
            | StreamDispose d -> d()
            | NotDisposable -> ()


    let alwaysBroadcast (_) = true
    let neverBroadcast (_) = false


    let handleStreamMsg(
        streamMsg: StreamMsg<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>, 
        state: StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>) 
        : StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState> =

        let now = DateTime.Now
        match streamMsg with
            | StreamMsg.StreamValue streamVal ->
                let v: 'streamVal =
                    match streamVal with
                    | DelayedVal f -> f()
                    | StreamVal sv -> sv
                List.iter(fun subscriber -> subscriber v) state.Subscribers
                state

            | StreamMsg.UpdateState (updater) ->
                let  newState = updater(state.State)    //note that this function may have side effects
                {state with State = newState}

            | StreamMsg.StreamSubscribe (clientRef, subscriber, statusUpdater) ->
                let subscriberMap = state.SubscriberMap.Add(clientRef, subscriber)
                let statusUpdateMap = state.StatusSubscriberMap.Add(clientRef, statusUpdater)
                let subscribers =
                    subscriberMap |> Map.fold(fun acc _subscriberID subscriber -> subscriber ::  acc ) List.empty
                    // why do we have a separate list of subscribers? tk - and do we need 2 separate maps?
                {state with Subscribers = subscribers; SubscriberMap = subscriberMap; StatusSubscriberMap = statusUpdateMap}

            | StreamMsg.StreamUnsubscribe subscriberID ->
                let subscriberMap = state.SubscriberMap.Remove(subscriberID)
                let statusUpdateMap = state.StatusSubscriberMap.Remove(subscriberID)
                let subscribers =
                    subscriberMap |> Map.fold(fun acc _subscriberID subscriber -> subscriber ::  acc ) List.empty
                {state with Subscribers = subscribers; SubscriberMap = subscriberMap; StatusSubscriberMap = statusUpdateMap}


            | StreamMsg.StreamStart dispatchWhen ->
                toConsole( sprintf "Stream START for %s" state.StreamName)
                // StreamStart won't do anything if Buffer strategy is NoDispatch - change StreamStart include an option of strategy with this message tk
                match state.StreamStatus with
                    | StreamStatus.Active (active, maybeComplete, _dt) ->
                        match active with
                        | ActiveStream.Paused ->
                            //create events between dt and now?
                            // let (_initialState, disposer) = state.Init()  // removed this from paused 14.2.22
                            // check if we still rely on head buffer to pause
                            Dispatch dispatchWhen |> (BufferMsg.SetStrategy >> state.HeadBuffer)
                            {state with StreamStatus = StreamStatus.Active (ActiveStream.Streaming, maybeComplete, now);}
                            
                        | ActiveStream.Initialising ->
                            //create events between dt and now?
                            //start timer
                            let (initialState, disposer) = state.Init()
                            Dispatch dispatchWhen |> (BufferMsg.SetStrategy >> state.HeadBuffer)
                            
                            {state with StreamStatus = StreamStatus.Active (ActiveStream.Streaming, None, now); StreamDisposer = disposer;  State = initialState}

                        | _ -> 
                            state  //already active no need to start.  possible to make this impossible?


                    | StreamStatus.Terminated _dt ->
                        state


            | StreamMsg.StreamPause ->
                match state.StreamStatus with
                    | StreamStatus.Active (ActiveStream.Streaming, maybeComplete, _dt) ->
                        // disposeOf(disposer) // do we actually want to do this when pausing? tk
                        // if we have a supplier buffer then set its strategy to NoDispatch
                        NoDispatch |> (BufferMsg.SetStrategy >> state.HeadBuffer)

                        {state with StreamStatus = StreamStatus.Active (ActiveStream.Paused, maybeComplete, now)}  //already active no need to start.  Make this impossible?

                    | _  ->
                        state  

            | StreamMsg.StreamTearDown ->
                //need to look into what is needed to tear eveything down
                //do subscribers need to be notified?
                //should this not be possible if there are existing subscribers? the  plan is to  have a buffer per subscriber which the client can tear down tk

                let newState =
                    match state.StreamStatus with
                        | StreamStatus.Terminated _dt ->
                            state
                        | _ ->
                            // we may need a recipe for this depending on dependency chain
                            // do we need to hear back from downstream before proceding with rest of teardown? tk
                            // we should be tearing down even if in Paused state tk
                            // server ref would have to be stored in the map along with clientRef? tk
                            Map.iter(fun clientRef subscriber -> subscriber (clientRef, "serverRef tk", state.StreamStatus, StreamStatus.Terminated now)) state.StatusSubscriberMap
                            disposeOf(state.StreamDisposer)

                            {state with StreamStatus = StreamStatus.Terminated now; Subscribers = List.empty; SubscriberMap = Map.empty; StatusSubscriberMap = Map.empty; BroadcastPredicate = (fun (_f) -> false); HeadBuffer = (fun (_bufMsg) -> ()) ;  }

                {newState with Subscribers = list.Empty;}

            | StreamMsg.SetInit init ->   //where the initialisation function needs to route messages to the owning mailbox
                {state with Init = init;}

            | StreamMsg.SetBuffer buffer ->
                {state with HeadBuffer = buffer;}

            | StreamMsg.StreamSourceStatusChange (tag, prevStreamStatus, newStreamStatus) -> // we probably need to also have the name of the provider
                //upstream data stream  status has changed.  we want to tell child nodes that the parent status has changed
                // except that we are also using it to set the state of the child which is not  the same thing tk
                // an issue that we were having  is that parent status change meant that child actors concluded that they were 
                // terminated.
                toConsole(sprintf  "Source Status change received by %s" state.StreamName)

                state.StatusSubscriberMap |>
                    // subscriberID is ClientRef, tag the label given to this stream by the server
                    Map.iter(fun subscriberID subscriber -> (subscriberID, tag, prevStreamStatus, newStreamStatus) |> subscriber)

                state



    let handleTimerMsg (streamMsg: StreamMsg<Timers.ElapsedEventArgs, Timers.ElapsedEventArgs, TimeSeriesValue<Timestamp>, list<TimeSeriesValue<Timestamp>>, unit, NO_STATE>) (state) =
        let streamState = handleStreamMsg(streamMsg, state)
        match streamMsg with
            | StreamMsg.StreamValue streamVal ->
                let vv =
                    match streamVal with
                    | DelayedVal f -> f()
                    | StreamVal sv -> sv
                match vv with 
                        | [] -> 
                            toConsole( "No timer value given which  should be impossible")

                        | h :: _ ->
                            toConsole( sprintf "Timer tick: %s" (h.Value.ToLongTimeString()) ) //GET THIS
                
            | StreamMsg.UpdateState _ -> toConsole( "Timer update state")
            | StreamMsg.StreamSubscribe _ -> toConsole( "Timer subscribe")
            | StreamMsg.StreamUnsubscribe _ -> toConsole( "Timer unsubscribe")
            | StreamMsg.StreamStart _dispatchWhen -> toConsole( "Timer Start")
            | StreamMsg.StreamPause -> toConsole( "Timer Pause")
            | StreamMsg.StreamTearDown -> toConsole( "Timer Tear down")
            | StreamMsg.SetInit _ ->     toConsole( "Set Timer Init")
            | StreamMsg.SetBuffer _ -> toConsole( "Set Timer Buffer")
            | StreamMsg.StreamSourceStatusChange (_subscriberID, _prevStreamStatus, _newStreamStatus) ->
                // There shouldn't be anything upstream of this timer
                toConsole(sprintf  "(SweetCaroline, ba ba ba) Source Status change received by %s" state.StreamName)
        streamState

    let makeEmitterMailbox<'msg, 'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>(msgHandler: 'msg -> StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState> -> StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>, streamState:StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>) =
        //the generator function will already be embedded into StreamState
        let genMailbox = MailboxProcessor<'msg>.Start(fun agent ->
            // Function that implements the body of the agent
            //the state here contains a list of subscribers
            //in reality there will only be one subscriber per scan class
            let rec loop (state: StreamState<'headBufIn, 'headBufInA, 'headBufOut, 'streamVal, 'streamState, 'bufState>) = async {
                // Asynchronously wait for the next message
                let! msg = agent.Receive()
                let newState = msgHandler msg state //newVal could be recorded on the state

                return! loop newState
            }

            // Start the body with initial values
            loop streamState)

        //pass back a function through which to access this data source
        fun(msg: 'msg) ->
            genMailbox.Post(msg)

    // data Types

    //     Incoming: bufIn
    //     Unpacked bufInA
    //     Dispatchable bufOUt
    // //workflow: dataIn -> unpack -> bufInA -> list -> generator -> bufOut -> dispatch
    // let makeHandlerOrig<'bufIn, 'bufInA, 'bufOut, 'bufState>
    //     (dispatcher: Subscriber<'bufOut>) 
    //     (inputHandler: BufferHandler<'bufInA, 'bufState>) 
    //     (unpack: Generator<'bufIn, 'bufInA>) 
    //     (_bufState: 'bufState) = //do we need this line? tk it potentially locks 'bufState without explicit typing
    //      fun (maybeGenerator: option<Generator<list<'bufInA>, 'bufOut>>) (maybeBufIn:option<'bufIn>, pending: list<'bufInA>, backlog:list<'bufInA>, bufState: 'bufState) ->
    //         let bProcess = Option.isSome maybeGenerator
    //         let maybeBufInA = maybeBufIn |> Option.map unpack

    //         let (forDispatch, newPending, newBacklog, newState) =
    //             inputHandler bProcess (maybeBufInA, pending, backlog, bufState) 

    //         maybeGenerator |> Option.iter (fun gen ->
    //             forDispatch |> (gen >> dispatcher)
    //         )
            
    //         (newPending, newBacklog, newState)

    let makeHandler<'bufIn, 'bufInA, 'bufOut, 'bufState>
        (handlerFor: string) //this should be the tag name tk this does not appear to be used
        (dispatcher: Subscriber<'bufOut>) 
        (downstreamStatusUpdater: Subscriber<TagAlias * StreamStatus * StreamStatus>) 
        (inputHandler: BufferHandler<'bufInA, 'bufState>) 
        (unpack: Generator<'bufIn * list<'bufInA> * 'bufState, list<'bufInA> * 'bufState>) 
        (_bufferState: 'bufState) = //do we need this line? tk it potentially locks 'bufState without explicit typing
         fun (maybeGenerator: option<Generator<list<'bufInA>, 'bufOut>>) (maybeBufIns:option<list<'bufIn>>, pending: list<'bufInA>, backlog:list<'bufInA>, bufState: 'bufState) ->
            let bProcess = Option.isSome maybeGenerator
            // if we have data it will be a list<'bufIn> Unpack each one produce new backlog:list<bufInA>
            let backlog', state' = 
                (maybeBufIns |> (Option.map ( fun bufIns->
                    bufIns |>
                    List.fold(fun ((bufInAsAcc:list<'bufInA>),  (buffStateAcc: 'bufState)) bufIn ->
                        unpack (bufIn, bufInAsAcc, buffStateAcc)
                    ) (backlog, bufState)
                ) ))
                |> (Option.defaultValue (backlog, bufState))

            // toConsole( sprintf "bProcess in %s is :%b, %d" handlerFor bProcess backlog'.Length)

            let (forDispatch, newPending, newBacklog, maybeStatusChange, newState) =
                inputHandler bProcess (pending, backlog', state') 

            //i think we had a version that propagated state changes downstream here WORKING HERE

            if not(forDispatch.IsEmpty) then
                maybeGenerator |> 
                Option.iter (fun gen ->
                    forDispatch |> (gen >> dispatcher)
                )

            match maybeStatusChange with
            | Some statusUpdate ->
                let bufName =
                    match tryUnbox<IStreamConsumer<'bufState>>(bufState) with                    
                    | Some isc ->
                        isc.GetName()
                    | _ -> "Unknown - does not implement IStreamConsumer"
                toConsole(sprintf  "-_Stream Status CHANGE for buffer: %s %A" bufName statusUpdate)
                statusUpdate |> downstreamStatusUpdater
            | None -> ()

            
            (newPending, newBacklog, newState)

    
    let handleBufferInput<'bufIn, 'bufState>
        (bDispatch: bool)
        (maybeInput: option<'bufIn>, 
        pending:list<'bufIn>, 
        backlog:list<'bufIn>, 
        bufState: 'bufState)
            : ('bufIn list * 'bufIn list * 'bufIn list * 'bufState) =

        // is this used? it ignores state makeTimerGenerator calls it with no state
        let newBacklog = 
            match maybeInput with
                | Some v -> 
                    v :: backlog
                | None -> backlog

        match bDispatch with
        | true ->
            match pending with
            | [] ->
                match List.rev newBacklog  with
                | [] -> //should be impossible as we have cons'd input onto backlog, but input could theoretially be empty.
                    [], [], [], bufState
                | h :: t ->
                    //how much to put into pending should be configurable for the moment just add the head
                    [h], t, [], bufState

            | h :: t ->
                //how much to put into pending should be configurable for the moment just add the head
                [h], t, newBacklog, bufState
        | false ->
            match pending with
            | [] ->
                let revBacklog = List.rev newBacklog
                [], revBacklog, [], bufState

            | p ->
                [], p, newBacklog, bufState


    // what we actually need is a transform function that takes 'bufIn and translates to 'bufinA - which is what gets held in pending and backlog
    let handleBufferInputTTSV<'bufIn, 'bufState>
        (bDispatch: bool)
        (maybeInput: option<TaggedValues<TimeSeriesValue<'bufIn>>>, pending:list<TimeSeriesValue<'bufIn>>, backlog:list<TimeSeriesValue<'bufIn>>, bufState: 'bufState)
            : (list<TimeSeriesValue<'bufIn>> * list<TimeSeriesValue<'bufIn>> * list<TimeSeriesValue<'bufIn>> * 'bufState) =

        let newBacklog =
            match maybeInput with
                | Some ttsv ->
                    let {Tag = _tag; Values = tsvs} = ttsv
                    tsvs |>
                    List.fold(fun acc i ->
                        i :: acc
                    ) backlog
                | None -> backlog

        match bDispatch with
        | true ->
            match pending with
            | [] ->
                match List.rev newBacklog  with
                | [] -> //should be impossible as we have cons'd input onto backlog, but input could theoretially be empty.
                    [], [], [], bufState
                | h :: t ->
                    //how much to put into pending should be configurable for the moment just add the head
                    [h], t, [], bufState

            | h :: t ->
                //how much to put into pending should be configurable for the moment just add the head
                [h], t, newBacklog, bufState
        | false ->
            match pending with
            | [] ->
                let revBacklog = List.rev newBacklog
                [], revBacklog, [], bufState

            | p ->
                [], p, newBacklog, bufState

    let topSlice(bufIn, _backlog, bufState)  =
        [bufIn], bufState


    let handleBufferInputAll<'bufIn, 'bufState>
        (bDispatch: bool)  // what is the point of this? tk
        (maybeInput: option<'bufIn>, pending:list<'bufIn>, backlog:list<'bufIn>, bufState: 'bufState)
            : ('bufIn list * 'bufIn list * 'bufIn list * 'bufState) =

        toConsole(sprintf  "In handle input all with dispatch %b data :%A " bDispatch maybeInput)

        //ignore pending in this mode - all inputs go to backlog and all backlog passed out on dispatch
        let newBacklog =
            match maybeInput with
                | Some v -> v :: backlog
                | None -> backlog

        match bDispatch with
        | true ->
            let jj = List.rev newBacklog, [], [], bufState

            jj

        | false ->
            [], [], newBacklog, bufState

    let handleBufferInputTopOnly<'bufIn, 'bufState>
        (bDispatch: bool)
        (maybeInput: option<'bufIn>, pending:list<'bufIn>, backlog:list<'bufIn>, bufState: 'bufState)
            : ('bufIn list * 'bufIn list * 'bufIn list * 'bufState) =

        // ignore backlog in this mode - all inputs go to pending replacing what was there before
        // note that AddToBufferBulk writes to backlog, so you can't bulk load with this handler, which does not make sense anyway
        let newPending =
            match maybeInput with
                | Some v ->[v]
                | None -> pending

        match bDispatch with
        | true ->
            newPending, [], [], bufState

        | false ->
            [], newPending, [], bufState

    let handleBufferBatch<'bufIn, 'bufState>
        (slice: (list<'bufIn> * list<'bufIn> * 'bufState) -> (list<'bufIn> * list<'bufIn> * list<'bufIn> * 'bufState)) // pending and backlog passed in, dispatchable, pending and newBacklog returned 
        (bDispatch: bool)
        (maybeInput: option<'bufIn>, 
        pending:list<'bufIn>, 
        backlog:list<'bufIn>, 
        bufState: 'bufState)
            : ('bufIn list * 'bufIn list * 'bufIn list * 'bufState) =

        // in this mode when we get a new value, we pass all the values that we have into condition
        // so it is similar to handleInputAll so far
        // then we pass this data into condition
        // so condition now has all the data, and this is partitioned into readyForDispatch and backlog
        // pending is used to store state, such as the oldest value


        let allData =
            match maybeInput with
                | Some v -> v :: backlog
                | None -> backlog

        match bDispatch with
        | true ->
            slice(allData, pending, bufState) //pending represents state and may always be empty
            
        | false ->
            [], [], backlog, bufState


    // // let initialiseBufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>(hbi:BufferHandler<'bufIn>, dispatcher: Subscriber<'bufOut>, dispatchStrategy: DispatchStrategy, generator: Generator<'bufInA, 'bufOut>, prep: Generator<list<'bufIn>, list<'bufInA>>, delay) =
    // let initialBuffStateOrig<'bufIn, 'bufInA, 'bufOut, 'bufState>(
    //     hbi:BufferHandler<'bufInA, 'bufState>, 
    //     dispatcher: Subscriber<'bufOut>, 
    //     dispatchStrategy: DispatchStrategy, 
    //     generator: Generator<list<'bufInA>, 'bufOut>, 
    //     delay, 
    //     unpack: 'bufIn -> 'bufInA, 
    //     bufState: 'bufState) =
    //     let pending = list<'bufInA>.Empty
    //     let backlog = list<'bufInA>.Empty


    //     //let hbi: BufferHandler<'bufIn> = handleBufferInput
    //     let bih = makeHandler dispatcher hbi unpack bufState

    //     // type BufferInputHandler<'bufIn, 'bufInA, 'bufOut, 'bufState> = 
    //     // option<Generator<list<'bufInA>, 'bufOut>> 
    //     // -> option<'bufIn> * 'bufInA list * 'bufInA list 
    //     // -> 'bufInA list * 'bufInA list


    //     {
    //         Generator = generator
    //         // Dispatcher = dispatcher // check where this is used and whether we need it now tk
    //         DispatchStrategy = dispatchStrategy
    //         BufferInputHandler = bih
    //         Pending = pending
    //         Backlog = backlog
    //         Delay = delay
    //         BufferState = bufState
    //     }


    let initialiseBufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>(
        // slicer: list<'bufInA> * list<'bufInA> * 'bufState * bool -> list<'bufInA> * list<'bufInA> * list<'bufInA> * 'bufState,
        bufName: string,
        buffHandler:BufferHandler<'bufInA, 'bufState>,
        dispatcher: Subscriber<'bufOut>, 
        dispatchStrategy: DispatchStrategy,  
        downStreamStatusUpdater:Subscriber<TagAlias * StreamStatus * StreamStatus>,
        generator: Generator<list<'bufInA>,'bufOut>, 
        delay: bool,  
        (unpack: Generator<'bufIn * list<'bufInA> * 'bufState, list<'bufInA> * 'bufState>),
        bufState: 'bufState)    
        : BufferState<'bufIn,'bufInA,'bufOut,'bufState> =
        // we are stipulating that data always goes to the backlog - is that reasonable?
        
        let pending = list<'bufInA>.Empty
        let backlog = list<'bufInA>.Empty

        //let hbi: BufferHandler<'bufIn> = handleBufferInput
        let bih = makeHandler bufName dispatcher downStreamStatusUpdater buffHandler unpack bufState


        {
            Generator = generator
            // Dispatcher = dispatcher // check where this is used and whether we need it now tk
            DispatchStrategy = dispatchStrategy
            BufferInputHandler = bih
            Pending = pending
            Backlog = backlog
            Delay = delay
            BufferState = bufState
            BufName= bufName
        }


    //++-----------------------------------------------------
    // functionally the same as createBufferBox but uses an AkkaActor
    // I would like to move this out into AkkaRouter and lose references to Akka packages, 
    // but StreamInfrastructure has a reference to that and has BufferMessage type declarations
    let createBufferBox2<'bufIn, 'bufInA, 'bufOut, 'bufState>
        (actorSystem: ActorSystem,
        bufName: string, 
        initialBufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =

        let whereAreWe = "streamInfrastructure createBufferBox"

        let bufferInputHandler
            (_mailbox: Actor<BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>>) 
            (msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) 
            (bufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =
            //update to return a result at some point
            match msg with
                BufferMsg.UpdateState updater ->
                    let bufferState' = updater(bufferState.BufferState)
                    {bufferState with BufferState = bufferState';}

                | BufferMsg.SetGenerator (generator, dispatchStrategy) ->
                    toConsole(sprintf "Set generator called on buffer %s" bufferState.BufName)
                    match dispatchStrategy with
                        | Dispatch dispatchWhen ->
                            match bufferState.Pending.Length + bufferState.Backlog.Length > 0 with
                            | true ->
                                let (newPending, newBacklog, newState) = //what is happening with newState tk tbd?
                                    bufferState.BufferInputHandler (Some generator) (None, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)

                                match dispatchWhen with
                                | AlwaysDispatch ->
                                    {bufferState with Generator = generator; DispatchStrategy = Dispatch AlwaysDispatch; Pending = newPending; Backlog = newBacklog; BufferState = newState;}

                                | SingleDispatch ->
                                    {bufferState with Generator = generator; DispatchStrategy = NoDispatch; Pending = newPending; Backlog = newBacklog; BufferState = newState;}
                            | false ->
                                {bufferState with Generator = generator; DispatchStrategy = dispatchStrategy}

                        | NoDispatch->
                            {bufferState with Generator = generator; DispatchStrategy = dispatchStrategy}

                | BufferMsg.AddToBuffer values ->
                    let maybeNewStrategy, maybeGen =
                        match bufferState.DispatchStrategy with
                        | NoDispatch -> 
                            // toConsole( sprintf "Strategy is no dispatch on buffer %s" bufferState.BufName )
                            None, None
                        | Dispatch dispatchWhen ->
                            // toConsole( sprintf "Strategy is DISPATCH on buffer %s" bufferState.BufName )
                            match dispatchWhen with
                            | AlwaysDispatch -> None, Some bufferState.Generator
                            | SingleDispatch -> Some NoDispatch, Some bufferState.Generator

                    let(newPending, newBacklog, newState) = bufferState.BufferInputHandler maybeGen (Some values, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                    // toConsole(sprintf  "For buffer %s After buffer handler : koyaniskatsu: %d, %d" bufName newPending.Length newBacklog.Length)
                    // toConsole(sprintf  "Maybe Strategy: %A : mmaybeGen: %A" maybeNewStrategy maybeGen)

                    match maybeNewStrategy with
                        | Some strategy -> {bufferState with DispatchStrategy = strategy; Pending = newPending; Backlog = newBacklog}
                        | None -> {bufferState with Pending = newPending; Backlog = newBacklog; BufferState = newState}


                | BufferMsg.AddToBufferBulk vList ->
                    let maybeNewStrategy, maybeGen =
                        match bufferState.DispatchStrategy with
                        | NoDispatch -> None, None
                        | Dispatch dispatchWhen ->
                            match dispatchWhen with
                            | AlwaysDispatch -> None, Some bufferState.Generator
                            | SingleDispatch -> Some NoDispatch, Some bufferState.Generator

                    let bLog =
                        vList |>
                        List.fold(fun acc v ->
                            v :: acc
                        ) bufferState.Backlog

                    let(newPending, newBacklog, newState) = bufferState.BufferInputHandler maybeGen (None, bufferState.Pending, bLog, bufferState.BufferState)

                    match maybeNewStrategy with
                        | Some strategy -> {bufferState with DispatchStrategy = strategy; Pending = newPending; Backlog = newBacklog; BufferState = newState}
                        | None -> {bufferState with Pending = newPending; Backlog = newBacklog; BufferState = newState}


                | BufferMsg.SetStrategy dispatchStrategy ->
                    let bufferHasData = bufferState.Pending.Length + bufferState.Backlog.Length > 0
                    // toConsole( sprintf "Setting strategy on buffer %s" bufferState.BufName)
                    let newStrategy, newPending, newBacklog, newState =
                        match dispatchStrategy with
                        | NoDispatch -> (NoDispatch, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                        | Dispatch dispatchWhen ->
                            let nextStrategy =
                                match dispatchWhen with
                                | AlwaysDispatch -> dispatchStrategy
                                | SingleDispatch ->
                                    // toConsole(sprintf  "Setting single dispatch for %s: %d:%d" bufName bufferState.Pending.Length  bufferState.Backlog.Length)
                                    // if there is no data in the buffer then strategy remains single dispatch, otherwise NoDispatch
                                    if bufferHasData then
                                        // toConsole( "With data so just this time")
                                        NoDispatch
                                    else
                                        (Dispatch SingleDispatch)
                            // need to save newState tk
                            let(newPending', newBacklog', newState') =
                                match bufferHasData with
                                | true ->
                                    bufferState.BufferInputHandler (Some bufferState.Generator) (None, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                                | false ->
                                    (bufferState.Pending, bufferState.Backlog, bufferState.BufferState)

                            (nextStrategy, newPending', newBacklog', newState')

                    {bufferState with DispatchStrategy = newStrategy; Pending = newPending; Backlog = newBacklog; BufferState = newState}

                | BufferMsg.TeardownBuffer ->
                    // is the teardown of a buffer a generic operation or specific to each one,
                    // in which case how will we manage that?
                    // if we are shutting down - we need to unsubscribe from any stream provider we might have
                    // how to do that?  our provider is anyone that is sending us AddToBuffer messages
                    // and also anyone sending SetStrategy messages, which is getting complicated.
                    // so we need a list of those and a way to unsubscribe
                    // hopefully we can postpone some or most of this for now.  The main aim at the moment
                    // is to close the file writer.
                    // it looks as if buffer teardonw will be specific to each case as some will have set strategy others add to buffer
                    // unless those operations came through a function that added the event sources to a list of providers.
                    // examples
                        // data coming into combiner buffer
                        // all buffers will have data sources (AddToBuffer) so we need a complementary function to unsubscribe
                        // the only way to do this will be to pass an id when subscribing
                        // combiner has multiple source that AddToBuffer - so we need to flag wherever we set that.

                        // the other message type was set strategy
                        // this might refer to the same source as add to buffer
                        // where set strategy is called from within dispatcher it can be ignored as
                        // the dispatcher is the same as the generator which depends on AddToBuffer messages

                        // so we need to update the subscription process so that we can unsubscribe
                    // let noOp = fun(_x) -> () // will generator ever have resources to clear?

                    {bufferState with DispatchStrategy = NoDispatch; Pending = list.Empty; Backlog = list.Empty;}

//------------------------------------


        // let emptyLocation  = {
        //     ActorPath = "";
        //     AddressResolution = AddressResolution.None;
        // }

        // we will need to incorporate an aspect of ActorLocation for actors whose existence is a dependency tk
        let bufferMsgHandlerID = Tweega.Utils.getTempID("bufferMsgHandler_")


        toConsole( sprintf "in createBufferBox, creating mailbox : %s\n" bufferMsgHandlerID)
        
        spawn actorSystem bufferMsgHandlerID <| fun (mailbox: Actor<BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>>) ->
            let rec loop (state: BufferState<'bufIn,'bufInA,'bufOut,'bufState>) = actor {
                let! (msg:BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) = mailbox.Receive()

                let newState = bufferInputHandler mailbox msg state
                return! loop newState
            }

            loop initialBufferState

    

    //++-------------------------------------------------------

    //this uses Agents instead of actors and should be replaced tk, particuarly for anything that needs supervision - see createBufferBox2
    let createBufferBox<'bufIn, 'bufInA, 'bufOut, 'bufState>(bufName: string, initialBufferState :BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =
        let bufferInputHandler (msg:BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) (bufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =
            //update to return a result at some point
            match msg with
                BufferMsg.UpdateState updater ->
                    let bufferState' = updater(bufferState.BufferState)
                    {bufferState with BufferState = bufferState';}

                | BufferMsg.SetGenerator (generator, dispatchStrategy) ->
                    match dispatchStrategy with
                        | Dispatch dispatchWhen ->
                            match bufferState.Pending.Length + bufferState.Backlog.Length > 0 with
                            | true ->
                                let (newPending, newBacklog, newState) = //what is happening with newState tk tbd?
                                    bufferState.BufferInputHandler (Some generator) (None, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)

                                match dispatchWhen with
                                | AlwaysDispatch ->
                                    {bufferState with Generator = generator; DispatchStrategy = Dispatch AlwaysDispatch; Pending = newPending; Backlog = newBacklog; BufferState = newState;}

                                | SingleDispatch ->
                                    {bufferState with Generator = generator; DispatchStrategy = NoDispatch; Pending = newPending; Backlog = newBacklog; BufferState = newState;}
                            | false ->
                                {bufferState with Generator = generator; DispatchStrategy = dispatchStrategy}

                        | NoDispatch->
                            {bufferState with Generator = generator; DispatchStrategy = dispatchStrategy}

                | BufferMsg.AddToBuffer values ->
                    let maybeNewStrategy, maybeGen =
                        match bufferState.DispatchStrategy with
                        | NoDispatch -> 
                            // toConsole( "Strategy is no dispatch")
                            None, None
                        | Dispatch dispatchWhen ->
                            // toConsole( "Strategy is DISPATCH ")
                            match dispatchWhen with
                            | AlwaysDispatch -> None, Some bufferState.Generator
                            | SingleDispatch -> Some NoDispatch, Some bufferState.Generator

                    let(newPending, newBacklog, newState) = bufferState.BufferInputHandler maybeGen (Some values, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                    // toConsole( sprintf "For buffer %s After buffer handler : koyaniskatsu: %d, %d" bufName newPending.Length newBacklog.Length)
                    // toConsole(sprintf  "Maybe Strategy: %A : mmaybeGen: %A" maybeNewStrategy maybeGen)

                    match maybeNewStrategy with
                        | Some strategy -> {bufferState with DispatchStrategy = strategy; Pending = newPending; Backlog = newBacklog}
                        | None -> {bufferState with Pending = newPending; Backlog = newBacklog; BufferState = newState}


                | BufferMsg.AddToBufferBulk vList ->
                    let maybeNewStrategy, maybeGen =
                        match bufferState.DispatchStrategy with
                        | NoDispatch -> None, None
                        | Dispatch dispatchWhen ->
                            match dispatchWhen with
                            | AlwaysDispatch -> None, Some bufferState.Generator
                            | SingleDispatch -> Some NoDispatch, Some bufferState.Generator

                    let bLog =
                        vList |>
                        List.fold(fun acc v ->
                            v :: acc
                        ) bufferState.Backlog

                    let(newPending, newBacklog, newState) = bufferState.BufferInputHandler maybeGen (None, bufferState.Pending, bLog, bufferState.BufferState)

                    match maybeNewStrategy with
                        | Some strategy -> {bufferState with DispatchStrategy = strategy; Pending = newPending; Backlog = newBacklog; BufferState = newState}
                        | None -> {bufferState with Pending = newPending; Backlog = newBacklog; BufferState = newState}


                | BufferMsg.SetStrategy dispatchStrategy ->
                    let bufferHasData = bufferState.Pending.Length + bufferState.Backlog.Length > 0
                    // toConsole( sprintf "Setting strategy on buffer %s" bufferState.BufName)
                    let newStrategy, newPending, newBacklog, newState =
                        match dispatchStrategy with
                        | NoDispatch -> (NoDispatch, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                        | Dispatch dispatchWhen ->
                            let nextStrategy =
                                match dispatchWhen with
                                | AlwaysDispatch -> dispatchStrategy
                                | SingleDispatch ->
                                    // toConsole(sprintf  "Setting single dispatch for %s: %d:%d" bufName bufferState.Pending.Length  bufferState.Backlog.Length)
                                    // if there is no data in the buffer then strategy remains single dispatch, otherwise NoDispatch
                                    if bufferHasData then
                                        // toConsole( "With data so just this time")
                                        NoDispatch
                                    else
                                        (Dispatch SingleDispatch)
                            // need to save newState tk
                            let(newPending', newBacklog', newState') =
                                match bufferHasData with
                                | true ->
                                    bufferState.BufferInputHandler (Some bufferState.Generator) (None, bufferState.Pending, bufferState.Backlog, bufferState.BufferState)
                                | false ->
                                    (bufferState.Pending, bufferState.Backlog, bufferState.BufferState)

                            (nextStrategy, newPending', newBacklog', newState')

                    {bufferState with DispatchStrategy = newStrategy; Pending = newPending; Backlog = newBacklog; BufferState = newState}

                | BufferMsg.TeardownBuffer ->
                    // is the teardown of a buffer a generic operation or specific to each one,
                    // in which case how will we manage that?
                    // if we are shutting down - we need to unsubscribe from any stream provider we might have
                    // how to do that?  our provider is anyone that is sending us AddToBuffer messages
                    // and also anyone sending SetStrategy messages, which is getting complicated.
                    // so we need a list of those and a way to unsubscribe
                    // hopefully we can postpone some or most of this for now.  The main aim at the moment
                    // is to close the file writer.
                    // it looks as if buffer teardonw will be specific to each case as some will have set strategy others add to buffer
                    // unless those operations came through a function that added the event sources to a list of providers.
                    // examples
                        // data coming into combiner buffer
                        // all buffers will have data sources (AddToBuffer) so we need a complementary function to unsubscribe
                        // the only way to do this will be to pass an id when subscribing
                        // combiner has multiple source that AddToBuffer - so we need to flag wherever we set that.

                        // the other message type was set strategy
                        // this might refer to the same source as add to buffer
                        // where set strategy is called from within dispatcher it can be ignored as
                        // the dispatcher is the same as the generator which depends on AddToBuffer messages

                        // so we need to update the subscription process so that we can unsubscribe
                    // let noOp = fun(_x) -> () // will generator ever have resources to clear?

                    {bufferState with DispatchStrategy = NoDispatch; Pending = list.Empty; Backlog = list.Empty;}

        let bufBox = MailboxProcessor<BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>>.Start(fun agent ->

            let rec loop (state) = async {
                // Asynchronously wait for the next message
                let! msg = agent.Receive()
                let newState = bufferInputHandler msg state

                return! loop newState
            }

            // Start the body with initial values
            loop initialBufferState)

        fun(msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) ->
            bufBox.Post(msg)


    let makeTimerGenerator(timerConfig: TimerConfig) =
        let gen = timerGen(timerConfig)


        let now = DateTime.Now
        let tempInit = fun() -> (), NotDisposable

        let streamState = {
            StreamState.StreamName = timerConfig.Name
            StreamState.StreamStatus = StreamStatus.Active (ActiveStream.Initialising, None, now)
            StreamState.State = () // use a NoState placeholder?
            StreamState.Subscribers = List.empty
            StreamState.StreamDisposer = NotDisposable
            StreamState.StatusSubscriberMap = Map.empty
            StreamState.SubscriberMap = Map.empty
            StreamState.Init = tempInit
            StreamState.BroadcastPredicate = alwaysBroadcast // this should be in config
            StreamState.HeadBuffer = fun (_bufMsg) -> () //: Subscription<BufferMsg<'bufIn, 'streamVal>>
        }
        let emitter = makeEmitterMailbox(handleTimerMsg, streamState)

        let subscriberToTimerTick =
            (fun (x) -> [x]) >> StreamVal >> StreamMsg.StreamValue >> emitter
 
    // unpack:Generator<'bufIn * list<'bufInA> * 'bufState, list<'bufInA> * 'bufState>) 
        
        // this could be generically labelled as topCopyUnpacker tk


        let timerBufState =
            initialiseBufferState("timer buffer", vanillaSlicer, subscriberToTimerTick, Dispatch AlwaysDispatch, statusUpdatesToEmitter emitter, gen, NODELAY, topSlice, NO_BUFFER_STATE)

        let timerBufBox =
            createBufferBox("timer Gen", timerBufState)

        let init = timerInit(timerConfig, timerBufBox)

        (StreamMsg.SetInit init) |> emitter  //so that when mailbox is restarted, the timer is also
        emitter


    // these 2 subscription point functions differ only in the data type of their subscriber arguments so we can hopefully rationalise
    let dataSubscriptionPoint 
        (streamAPIRouter: MessageHandler<StreamAPI>) 
        (
            clientRef: SubscriberID, 
            subscriber: Subscriber<list<TaggedValues<TimeSeriesValue<float>>>>, 
            statusUpdater: Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>
        ) = 
        // this only exists to enforce msg type before dispatch
        // assemble the subscription message for the data source

        // this will be a function that is passed to the writer - which passes in typed subscriber

        //console.log toConsole( "Calling subscription point bmn"

        let boxedSubscriber = box subscriber

        let dataSubscriptionMsg: StreamAPI =
            StreamAPI.StreamSubscribe (clientRef, boxedSubscriber, statusUpdater)
        
        // pass subscription request on to stream provider
        dataSubscriptionMsg |> streamAPIRouter


    let dataSubscriptionPoint2 
        (streamAPIRouter: MessageHandler<StreamAPI>) 
        (
            subscriberID: SubscriberID, 
            //Subscriber<list<TaggedValues<TSVsStatus<obj>>>>
            subscriber: Subscriber<list<TaggedValues<TSVsStatus<TimeSeriesValue<float>>>>>, 
            statusUpdater: Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>
        ) = 
    
        let boxedSubscriber = box subscriber

        let dataSubscriptionMsg: StreamAPI =
            StreamAPI.StreamSubscribe (subscriberID, boxedSubscriber, statusUpdater)
        
        // pass subscription request on to stream provider
        dataSubscriptionMsg |> streamAPIRouter

    //rationalises previous data subscription point functions into this tk
    let dataSubscriptionPoint3<'Data> 
        (streamAPIRouter: MessageHandler<StreamAPI>) 
        (
            subscriberID: SubscriberID, 
            //Subscriber<list<TaggedValues<TSVsStatus<obj>>>>
            subscriber: Subscriber<list<TaggedValues<'Data>>>, 
            statusUpdater: Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus>
        ) = 
    
        let boxedSubscriber = box subscriber

        let dataSubscriptionMsg: StreamAPI =
            StreamAPI.StreamSubscribe (subscriberID, boxedSubscriber, statusUpdater)
        
        // pass subscription request on to stream provider
        dataSubscriptionMsg |> streamAPIRouter

    let subscribeToSource4<'Data>
        // used by web sockets client manager and proxy - rationalise with other versionsof this function
        (streamSource: MessageHandler<StreamAPI>)
        (   subscriberID: SubscriberID,
            subscriber: Subscriber<list<TaggedValues<'Data>>>,
            statusUpdater: Subscriber<SubscriberID * StreamStatus * StreamStatus>) =

        // status updater is inconsistent - some versions have ClientRef, others not tk

        let statusUpdater' =
            fun(_clientRef:ClientRef, subscriberID:SubscriberID, streamStatus1: StreamStatus, streamsStatus2: StreamStatus) ->
                statusUpdater(subscriberID, streamStatus1, streamsStatus2)

        toConsole(sprintf  "Calling subscription point mnb for subscriber %s"  subscriberID)
        let subscriberType = Tweega.Utils.getTypeStr(subscriber.GetType())
        toConsole(sprintf  "boxing type %s" subscriberType)

        let boxedSubscriber = box subscriber

        let dataSubscriptionMsg: StreamAPI =
            StreamAPI.StreamSubscribe (subscriberID, boxedSubscriber, statusUpdater')

        // pass subscription request on to stream provider
        dataSubscriptionMsg |> streamSource

        // start the stream immediately -  review how stream starts should be handled tk - only makes sense if buffer per connsumer
        let m:StreamAPI = StreamAPI.StreamStart (DispatchWhen.AlwaysDispatch)
        m |> streamSource

        

    let createSlicedStreamAPI<'bufIn, 'bufInA, 'bufOut, 'bufState>
        (tag: TagAlias)
        (slicer: bool -> list<'bufInA> * list<'bufInA> * 'bufState -> list<'bufInA> * list<'bufInA> * list<'bufInA> * option<TagAlias * StreamStatus * StreamStatus> * 'bufState)
        (unpack: Generator<'bufIn * list<'bufInA> * 'bufState, list<'bufInA> * 'bufState>) 
        (generator:Generator<list<'bufInA>, 'bufOut>) 
        (bufferState: 'bufState)
        : Subscriber<list<'bufIn>> * MessageHandler<TypedStreamAPI<'bufOut>> =

        // pass in buffer name - it is not always chabbithog tk
        
        let handleStreamProxyMsg (streamMsg) (state) =
            let streamState = handleStreamMsg(streamMsg, state)
            match streamMsg with
                | StreamMsg.StreamValue _streamVal -> toConsole( "value received in tag pipe")
                | StreamMsg.UpdateState _ -> toConsole( "Tag update state")
                | StreamMsg.StreamSubscribe _ -> toConsole( "Tag subscribe qlp")
                | StreamMsg.StreamUnsubscribe _ -> toConsole( "Tag subscribe")
                | StreamMsg.StreamStart _dispatchWhen -> toConsole( "Tag Start")
                | StreamMsg.StreamPause -> toConsole( "Tag Pause")
                | StreamMsg.StreamTearDown -> toConsole( "Tag Tear down")
                | StreamMsg.SetInit _ ->     toConsole( "Set Tag Init")
                | StreamMsg.SetBuffer _ -> toConsole( "Set Tag Buffer")
                | StreamMsg.StreamSourceStatusChange (subscriberID, _prevStreamStatus, _newStreamStatus) ->
                    // toConsole( "(chabbithog) Souce Status change received by %s: for tag %s" state.StreamName tag
                    toConsole(sprintf  "Are we supposed to do something here? jkl message from: %s?" subscriberID)
                    //  we are a calc input stream  our source has had a staus chenge
                    // this is not critical but we should hook it up all the same
                    ()

            streamState

        let tempInit = fun() -> (), NotDisposable
        let now = DateTime.Now

        let streamState = {
            StreamState.StreamName = tag
            StreamState.StreamStatus = StreamStatus.Active (ActiveStream.Initialising, None, now)
            StreamState.State = () //use a NoState placeholder?
            StreamState.Subscribers = List.empty
            StreamState.StreamDisposer = NotDisposable
            StreamState.Init = tempInit
            StreamState.StatusSubscriberMap = Map.empty
            StreamState.SubscriberMap = Map.empty
            StreamState.BroadcastPredicate = alwaysBroadcast
            StreamState.HeadBuffer = fun (_bufMsg) -> (toConsole( sprintf  "Error: we should not see this. Message sent to headbuffer , but no Head buffer set"))
        }
        let emitter = makeEmitterMailbox(handleStreamProxyMsg, streamState)

        let dispatchToEmitter =            
            (StreamVal >> StreamMsg.StreamValue >> emitter)

        //slicer: bool -> list<'bufInA> * list<'bufInA> * 'bufState -> list<'bufInA> * list<'bufInA> * list<'bufInA> * option<TagAlias * StreamStatus * StreamStatus> * 'bufState)

        // controlling stream status (dispatch strategy) only makes sense if consumer has control over the stream
        // in other words the stream is not being fanned out to multiple consumers - in which case there needs to
        // be a buffer per consumer tk
        let streamBufState =
            initialiseBufferState("Chabbithog buffer", slicer, dispatchToEmitter, NoDispatch, statusUpdatesToEmitter emitter, generator, NODELAY, unpack, bufferState)

        let streamBufBox =
            createBufferBox("Stream API Buffer for Chabbithog", streamBufState)

        let writeToBuffer = 
            fun m ->
                // toConsole( sprintf "Papageno buff")
                m |> (BufferMsg.AddToBuffer >> streamBufBox)

        
        // what to do with this tk?  Do we need prevStatus to always be passed in? tk
        let statusUpdater = fun (clientRef,_serverRef, _prevStatus, newStatus) ->
            toConsole(sprintf  "jelly roll : %s -  %s" clientRef tag)
            // we need to send a message to update buffer state
            //make this a utility function tk
            let stateUpdater =
                fun(bufState) ->
                    match tryUnbox<IStreamConsumer<'bufState>>(bufState) with                    
                    | Some isc -> 
                        toConsole ( sprintf "Setting input status for (%s) to %A" clientRef newStatus)
                        isc.SetInputStatus(clientRef, newStatus)

                        // having set the input status do we not need to check how  that affects us?
                        // is this an important  message though
                        // it would be important if it indicated data complete?
                        // this is a case where parent status is effectively ours too
                        // if I am getting data in the combiner then do we not have data complete 
                        //of have we set this regardless?

                        // the problem for data in the combiner is that we have bSend set to false the whole time
                    | _ -> 
                        toConsole( sprintf "Error: Unable to cast buffer to IStreamConsumer in createSlicedStreamAPI for tag %s" tag)
                        bufState

            stateUpdater |> (BufferMsg.UpdateState >> streamBufBox)

            // we don't get this message
            // this is propagating the message. We received this from parent and are forwarding to children
            // what we need to do is evaluate what this means for us.
            // except we weren't coming here anyway,though we might  now

            // msg |> maybeStatusUpdater |>
            // Option.iter (StreamMsg.StreamSourceStatusChange >> emitter)
            
        // return an api that does not allow for new values to be inserted into the emitter tk
        // we have this code twice in this file - is that correct? tk
        let streamAPIHandler =
            fun streamAPIMsg ->
                match streamAPIMsg with
                | TypedStreamAPI.StreamSubscribe (clientRef, subscriber, statusUpdater) ->
                    (clientRef, subscriber, statusUpdater) |> (StreamMsg.StreamSubscribe >> emitter)
                | TypedStreamAPI.StreamUnsubscribe _subscriberID ->
                    //there is no mapping for unsubscribe for some reason tk
                    ()
                | TypedStreamAPI.StreamTeardown ->
                    BufferMsg.TeardownBuffer |> streamBufBox
                    StreamMsg.StreamTearDown |> emitter
                | TypedStreamAPI.StreamStart dispatchWhen ->
                    dispatchWhen |> (StreamMsg.StreamStart >> emitter)
                | TypedStreamAPI.StreamPause ->
                    StreamMsg.StreamPause |> emitter
                | TypedStreamAPI.StreamPull ->
                    //what to do here strategy to single tk
                    ()
                | TypedStreamAPI.StreamSourceStatusChange (clientRef, serverRef, prevStatus, newStatus) ->
                    toConsole( "Brother January")
                    (clientRef, serverRef, prevStatus, newStatus) |> statusUpdater  //goes to jelly roll

        // set the buffer as head buffer so that emitter can relay strategy changes (make this a parameter option? tk)
        streamBufBox |> (StreamMsg.SetBuffer >> emitter)

        (writeToBuffer, streamAPIHandler)
    
    