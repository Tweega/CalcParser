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


    let createStreamSourceBuffer() = 
        // stream source will supply ResolvedValues as lists containing single ResolvedValues
        // higher upstream we will need a buffer that takes in tsvs<float> etc
        // and spit them out as resolved values
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
    


    