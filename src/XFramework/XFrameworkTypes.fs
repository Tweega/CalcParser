namespace Tweega.Shared

open System

module XFrameworkTypes =
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
    type FilePath = string
    type WriterID = string  //StreamID more generic?
    type ClientRef = string
    type ServerRef = string
    type CycleCount = int
    type ErrorCount = int
    type RecordsWritten = int
    type WriteWarnings = list<string>

    type KVP = { key:string; value:string }

    type TaggedKVPs = {
        Tag: string;// this will actually be the function name, possibly qfn - for each function that we want,these are the options. tk
        KVPs: list<KVP>;
    }
        

    [<RequireQualifiedAccess>]
    type WriteResult =
        | WriteOK of RecordsWritten * WriteWarnings
        | WriteError of string

    type TTSVs<'Data> = list<TaggedValues<TimeSeriesValue<'Data>>>

    type SubscriberRef = string
    type MessageHandler<'Msg> = 'Msg -> unit
    type Subscriber<'T> = 'T -> unit


    // the original plan was to make the file storage extensible so as to bring new data streams online dynamically
    // but that will have to be a separate project when the need arises
    [<RequireQualifiedAccess>]
    type DataStore =
        | PIAF
        | File
        | WS // this should go on a DataStoreInternal DU

    [<RequireQualifiedAccess>]
    type PrimitiveType =
        | Float
        | Int
        // | String
        // etc

    // replace these with type codes tk - may not be so easy - per stack overflow - https://stackoverflow.com/questions/7329742/when-should-system-type-be-inherited
        // you can’t create instances (of classes derived from Type) using Activator.CreateInstance(yourSubType).
    // [<RequireQualifiedAccess>]
    // type StreamType =
    //     | Primitive of PrimitiveType
    //     | WriteResult of WriteResult
    //     | Custom of string

    type StorageInfo = {
        DataStore: DataStore
        WriterID: WriterID
    }


    // Credentails can be extended to include no credentials, impersonated etc. tk
    [<RequireQualifiedAccess>]
    type UserCredentials =
        | LoggedOnUser
        | Credentials of list<KVP>   //KVPs will end up being some kind of certificate


    // type AFDatabaseName = string
    // type PIServerName = string

    // // what is this doing here? tk should not have source specific types in XFrameworkTypes
    // [<RequireQualifiedAccess>]
    // type PICxnPoint  =
    //     | AF of PIServerName * AFDatabaseName
    //     | PI of PIServerName * PrimitiveType


    type CxnInfo = {
        CxnOptions: list<KVP>
        UserCredentials: UserCredentials
    }

    // this stream stuff should not be here - see issue in https://github.com/Tweega/ElmishD3/issues/17

    type CancelledReason =
        | JustBecause of string

    type FailureReason =
            | ExecutionError of string
            | Cancelled of CancelledReason
            | AuthenticationError of string
            | AccessError of string
            | OtherReason of string

    type AnnotationRecord = {
        Annotation: option<string>
        Creator: option<string>
        CreationDate: option<DateTime>
    }

    type ValueRecord = {
        AttrType: string
        AttrValue: string
    }

    type TypeQualifierRecord = {
        QualifierType: string
        QualifierValue: string
    }

    type EventFrameRefRecord = {
        ReferenceType: option<string>
        EFRef: string
    }

    type DataReferenceRecord = {
        DataReference: string
        DataRefID: string
    }

    type ElementRecord = {
        RootPath: string
        AttributeName: string
        MaybeDescription: option<string>
        MaybeIsHidden: option<bool>
        MaybeIsManualDataEntry: option<bool>
        MaybeIsConfigurationItem: option<bool>
        MaybeIsExcluded: option<bool>
        MaybeAttributeTrait: option<string>
        MaybeDefaultUOM: option<string>
        MaybeDisplayDigits: option<int>
        MaybeAttributeType: option<string>
        MaybeTypeQualifier: option<TypeQualifierRecord>
        MaybeStatus: option<string>
        MaybeTimestamp: option<DateTime>
        MaybeAttrValue: option<ValueRecord>
        MaybeDataReference: option<DataReferenceRecord>
        MaybeConfigString: option<string>
    }

    type AttributeRecord = {
        RootPath: string
        AttributeName: string
        MaybeDescription: option<string>
        MaybeIsHidden: option<bool>
        MaybeIsManualDataEntry: option<bool>
        MaybeIsConfigurationItem: option<bool>
        MaybeIsExcluded: option<bool>
        MaybeAttributeTrait: option<string>
        MaybeDefaultUOM: option<string>
        MaybeDisplayDigits: option<int>
        MaybeAttributeType: option<string>
        MaybeTypeQualifier: option<TypeQualifierRecord>
        MaybeStatus: option<string>
        MaybeTimestamp: option<DateTime>
        MaybeAttrValue: option<ValueRecord>
        MaybeDataReference: option<DataReferenceRecord>
        MaybeConfigString: option<string>
    }


    type EventFrameRecord = {
        EFName: string;
        Depth: int;
        MaybeParent: option<string>;
        MaybeReferenceType: option<string>
        MaybeAcknowledgedBy: option<string>;
        MaybeAcknowledgedDate: option<DateTime>;
        MaybeAreValuesCaptured: option<bool>;
        MaybeAnnotations: option<list<AnnotationRecord>>;
        MaybeAttributes: option<list<AttributeRecord>>;
        MaybeCanBeAcknowledged: option<bool>;
        MaybeCategories: option<list<string>>;
        MaybeDescription: option<string>;
        MaybeEndTime: option<DateTime>;
        MaybeIsAcknowledged: option<bool>;
        MaybeIsAnnotated: option<bool>;
        MaybeIsLocked: option<bool>;
        MaybeReferencedElements: option<list<string>>;
        MaybeReferencedEventFrames: option<list<EventFrameRefRecord>>;
        MaybeSeverity: option<int>;
        MaybeStartTime: option<DateTime>;
        MaybeTemplate: option<string>;
    }

    type ResourceID = int

    // type MainstreamRouter =  (StreamType * obj) -> unit
    type Either<'Left, 'Right> =
    | Left of  'Left
    | Right of 'Right
