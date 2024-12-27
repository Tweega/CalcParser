namespace Tweega.Shared
// open Tweega.Shared
open Tweega.Shared.ServerStreamTypes
open Tweega.Shared.ClientStreamTypes
open Tweega.Shared.XFrameworkTypes
open Tweega.Discovery.Shared.Types

module WriterTypes =

    // type CycleCount = int
    // type ErrorCount = int
    // type RecordsWritten = int
    // type WriteResult =
    //     | WriteOK of RecordsWritten
    //     | WriteError of string


    type WriterCollectiveID = string
    type APIid = string

    type WriterConfig = list<KVP>

    type WriterDisposer = unit -> unit
    type WriterStats = { //for the emitter
        CycleCount: CycleCount;
        ErrorCount: ErrorCount;
        RecordsWritten : RecordsWritten;
    }

    type WriterEmitterState = { //for the emitter
        WriterStatus : StreamStatus;
        WriterStats: WriterStats
    }

    type WriterRequestID = string
    type WriterAPI = WriterAPIui

    type WriteFails = list<DataStore * WriterID * list<FailureReason>>
    //when registering a stream source, i provide a function that takes
        // a callback to return a stream doc
        // a stream doc containing query

    // type WriterStorageMaybeObsolete = {
    //     TypeStorage: TypeStorage;
    //     WriterID: WriterID;
    // }


    // [<RequireQualifiedAccess>]
    // type WriterAPIInternal<'T> = //internal sounds more private than this is tk
    //     | StartWriter
    //     | StopWriter
    //     | SubscribeWriter of Subscriber<WriteResult>
    //     | TeardownWriter
    //     | AddDataSource of Subscriber<Subscriber<'T> * Subscriber<StreamStatus>> // tk add a feedback function to stop flow to writer
    //     //| UnsubscribeWriter of SubscriberID
    //     //| TruncateFile

    // [<RequireQualifiedAccess>]

    type WriterResultWithDataStore<'Data> = DataStore * WriterID * Result<MessageHandler<WriterAPIInternalTSV<'Data>>, list<FailureReason>>
    type WriterRequester<'Data> = Subscriber<list<WriterResultWithDataStore<'Data>>> * list<StorageInfo>
    type WriterBuilder<'Data> = Subscriber<WriterRequester<'Data>>
    //type WriterCallback = WriterRequestID * RecipeTypes.RecipeResult<list<WriterRequestID * list<TagAlias * Result<MessageHandler<WriterAPI>, FailureReason>>>>

    type WriterDoc<'Data> = {
        WriterRequests: list<StorageInfo>;
        WriterResults: list<WriterResultWithDataStore<'Data>>
    }

    // type CollectiveDoc<'Data> = {
    //     StorageInfo: StorageInfo
    //     CollectiveAPI: MessageHandler<WriterCollectivesAPI<'Data>>
    // }

    type WriterSpecs<'Data> = Subscriber<list<WriterResultWithDataStore<'Data>>> * list<StorageInfo>

    [<RequireQualifiedAccess>]
    type WriterBuilderMsg<'Data> =
        | BuildWriters of WriterSpecs<'Data>
        | RegisterWriterBuilder of (DataStore * WriterBuilder<'Data>)
        //| StreamSubscriptionRequests of Subscriber<list<StreamResultWithFunctionName<'Data>>> * list<StreamQuery>
        | UnregisterWriterBuilder of DataStore

    //SocketBuffer above has bufIn and bufInA related to TaggedValues<float and bufOut is a function from unit to data.  defined by generator below.
    type CombinerGenerator<'T> = (Generator<list<'T>, Generator<unit, list<'T>>> * DispatchStrategy -> SocketBufferMsg<'T>)

    type WriteResultStream<'Data> =  StreamMsg<list<TaggedValues<TimeSeriesValue<'Data>>>, list<TaggedValues<TimeSeriesValue<'Data>>>, WriteResult, WriteResult, WriterEmitterState,NO_STATE>
    //\\?type TaggedValuesStream<'Data> = StreamMsg<list<TaggedValues<TimeSeriesValue<'Data>>>, list<TaggedValues<TimeSeriesValue<'Data>>>, WriteResult, WriteResult, WriterEmitterState, NO_STATE>
    type TaggedValuesBuffer<'Data, 'WriteData> = BufferMsg<TaggedValues<TimeSeriesValue<'Data>>, TaggedValues<TimeSeriesValue<'Data>>, 'WriteData, NO_STATE>
    
    //check that this is correct tbd tk
    type ValuesBuffer<'Data, 'WriteData> = BufferMsg<list<TimeSeriesValue<'Data>>, list<list<TimeSeriesValue<'Data>>>, 'WriteData, unit>

    //WS\\type WriteResultStreamWS = StreamMsg<WebSocketServerMessage, list<WebSocketServerMessage>, WriteResult, WriteResult, WriterEmitterState>
    //WS\\type WSDataStream = StreamMsg<list<WebSocketServerMessage>, list<WebSocketServerMessage>, WriteResult, WriteResult, WriterEmitterState>
    //WS\\type WSDataBuffer = BufferMsg<WebSocketServerMessage, list<WebSocketServerMessage>, list<WebSocketServerMessage>> //is bufOut correct?

    //put functions in a separate utilities file? tk
    let now = System.DateTime.Now

    let createEmptyWriterState() =
        {
            WriterStatus = StreamStatus.Active (ActiveStream.Initialising, None, now);
            WriterStats = {
                CycleCount = 0;
                ErrorCount = 0;
                RecordsWritten = 0;
            }
        }
    type WriterState = {
        Status: StreamStatus
        DataSources: Set<SubscriberID>
    }

    type FileWriterState = {
        Serialiser: TagAlias -> list<TimeSeriesValue<float>> -> list<string>
    }