namespace Tweega.Discovery.Shared

module Types =
    open Tweega.Shared.XFrameworkTypes
    open Tweega.AkkaRouter.Types
    open Tweega.Shared.ClientStreamTypes
    open Tweega.Utils
    open System
    // open Tweega.
    // open Tweega.MainstreamT.MainstreamTSharedTypes

    type RequestID = string
    type DataType = string  // have to put some thought into how to represent data types safely - class GUIDs?
    type DeploymentID = string
    type StreamAPIobj = obj

    // type BuilderSignature = string
    type BuilderAlias = string
    type StreamTypeAlias = string


    // we have 2 addresses -
        // one for the actor that will deploy the builder
        // and the other for the deployed builder - when it is known.
        // It is possible for the known location to be supplied by application at start up for well known types
        // when first registering, but normally the deployer will be remote
        // if the type is well known, such as float, then the registrant of a data source need not
        // supply the location of the type builder.  In this case it supplies

    // when a consumer requests a stream, it passes in ids that can resolve to streams
    // for raw data that will be a single

    // builders construct the instrastructure for streams of particular types
    // 'raw' streams contain actual data and have a function signature of a single type, ie float, or EventFrame
    //


    // these are the messages that might be directed to a stream builder but we have to send boxed versions of them
    // type MainstreamBuilderMsgObj =
    //     | BuildStreams of Subscriber<list<StreamResultWithFunctionName<'Data>>> * list<StreamQuery>
    //     | RegisterStreamBuilder of (StreamSource * MessageHandler<StreamRequester<'Data>>)
    //     | UnregisterStreamBuilder of StreamSource

    // a consumer that knows the types of data that it will be connecting to will pass a message to be forwarded to
    // the mainstreamT that can supply the data

    // we will have 2 categories of functions? - actually this looks like a single type.
        // one that connects a data source with a consumer
        // and one that results in another calculation

    // nomenclature
    // builders are for the most part remote - they can also be called deployers
    // mainstreamT are what builders deploy.  They are containers for data sources of a particular type
    // mainstreamTs have a collection of implementations .. in some cases these are data sources in others calculations
    // so perhaps it is best to avoid use of the term builders - a builder is just a function called by the mainstreamT BuildStreams api

    // data sources are generically functions of some signature type, ie float -> float
    // to distinguish actual data sources from functions of 'T ->'T, their identifiers consist of only one data type
    // type MainstreamTAlias = string
    // type MainstreamTSignature = list<StreamType>

    [<RequireQualifiedAccess>]
    type DeployerAddress = 
        | CustomAddress of ActorLocation
        | Sender
    
    type DiscoveryAddresses = {
        DeployerAddress: ActorLocation // a function that takes a callback function ()possibly an actor ref to discovery actor.  When run will deploy the mainstreamT builder, then notify result through the callbacjk
        MainstreamTAddress:  ActorLocation
    }


    // type MainstreamMapping = Map<string, StreamFunctionSignature>

    [<RequireQualifiedAccess>]
    type StreamType2 = string    // this will map to a guid

    // [<RequireQualifiedAccess>]
    // type StreamExpression = 
    //     | StreamFunction of StreamFunction * TagAlias
    //     | StreamExpression of list<StreamExpression> // * Alias


    // [<RequireQualifiedAccess>]
    // type SE = 
    //     | Parseable of string
    //     | StreamExpression of StreamExpression

    // this is probably already defined

    [<RequireQualifiedAccess>]
    type InputApplicationOutput = // the result of a applying inputs to afunction tree will be one of 
        | StreamAPI of Subscriber<obj> // a StreamAPI wrapped in a function that takes an object,  casts and delivers it to the right actor that the builder has created
        // | FunctionTree of FunctionInfo * Subscriber<FunctionTree * Subscriber<BuildInfrastructureResult>> // another function tree
        // | DynamicFunctionTree of FunctionInfo  * Subscriber<FunctionTree * Subscriber<BuildInfrastructureResult>> * option<Subscriber<obj>> // both the above


    // type StreamAPIRouter = Subscriber<obj>
    
    // [<RequireQualifiedAccess>]
    // type BuildOutput = // the result of a building operation will be one of 
    //     | StreamAPI of Subscriber<obj> // a StreamAPI wrapped in a function that takes an object,  casts and delivers it to the right actor that the builder has created
    //     | DynamicFunctionTree of FunctionInfo * ProviderName * option<Subscriber<obj>> 

    // type BuildInfrastructureResult = Result<StreamAPIRouter, FailureReason> // add functionName to failure case tk

    
    type OptionsParameter = 
        | ResolvedOptions of Options
        | UnresolvedOptions


    type FunctionAlias = string  // name of function as stored by Discovery which will map to streamType and Functionanme

    type FunctionTree =
        | StreamSource of QualifiedFunctionName * OptionsParameter  //no stream type? tk
        | StreamFunc of StreamType * QualifiedFunctionName * list<NamedStreamParameter> * option<OptionsParameter> // functions to have options? tk
        | Dynamic of StreamType * StreamType * QualifiedFunctionName * list<NamedStreamParameter> * option<OptionsParameter> // a dynamic function always has an Unresolved final parameter

    and StreamParameter = 
        | Unresolved of StreamType
        | Resolved of FunctionTree

    and NamedStreamParameter = {ParamName:string; StreamParameter:StreamParameter}
    type StreamAPIRouter = Subscriber<obj>

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


    [<RequireQualifiedAccess>]
    type StreamAPI = //  the same as TypedStreamAPI but for the function manager to make request without knowing type so it can process lists of inputs of different types
    | StreamSubscribe of SubscriberID * obj * Subscriber<ClientRef * ServerRef * StreamStatus * StreamStatus> //live stream request
    | StreamUnsubscribe of SubscriberID
    | StreamTeardown    //who should be allowed to do this? tk REMOVE this tk it is not a client side thing.
    | StreamStart of DispatchWhen
    | StreamPause
    | StreamPull

    type ResolvedOptionsParameter = 
        | ResolvedOptionsParameter of Options
        
    
    type BuildInfrastructureResult = QualifiedTag * Result<MessageHandler<StreamAPI>, FailureReason>

    type Generator2<'T, 'U> = 'T -> 'U // this is duplicated in serverstreamtypes tk

    type ResolvedFunctionTree =
        | ResolvedStreamSource of QualifiedFunctionName * ResolvedOptionsParameter
        | ResolvedStreamFunc of StreamType * QualifiedFunctionName * list<ResolvedParameter> * option<ResolvedOptionsParameter> // functions to have options? tk
        | ResolvedDynamic of StreamType * StreamType * QualifiedFunctionName * list<ResolvedParameter> * option<ResolvedOptionsParameter> // a dynamic function always has an Unresolved final parameter
    
    and ResolvedParameter = {ParamName:string; FBI: FunctionBuilderInfo}
        // | ResolvedParameter of String * FunctionBuilderInfo

    and FunctionBuilderInfo = {
        ResolvedFunctionTree: ResolvedFunctionTree
        Builder: StreamAPIBuilder
    }
    
    // (QualifiedTag * MessageHandler<StreamAPI>) list

    and StreamAPIBuilder = Subscriber<ResolvedFunctionTree * Subscriber<BuildInfrastructureResult>>


    // type BuildStreamAPIMsg = 
    //     | BuildStreamAPI of ResolvedFunctionTree * Subscriber<BuildInfrastructureResult>  // when we want an instance of a proxy function

    type FunctionParamDoc = {
        QName: QualifiedFunctionName;
        Input: FunctionBuilderInfo;
        Output: BuildInfrastructureResult;
        ParamName: string; 
    }

    type BuilderDoc = {
        QName: QualifiedFunctionName;
        Options: Options;
        Credentials: option<CxnInfo>; //unless Credentials are the preserve of the specialist builder tk
        Params: List<FunctionParamDoc>;
        BIRs: list<BuildInfrastructureResult>  // one of these per parameter
        MaybeStreamAPI: option<BuildInfrastructureResult>
    }

    // type JJJ = {
    //     BuildStreamAPIMsg: BuildStreamAPIMsg
    //     MaybeBuilderDoc: option<BuilderDoc>; // output
    // }

    // this was here to limit api scope available to interactor (generic builder) to not call ProxyMsg.Initialise tk
    // [<RequireQualifiedAccess>]
    // type ProxyBuilderMsg =
    //     | BuildStreamAPI of BuilderDoc * Subscriber<BuildInfrastructureResult>
    //     // | Teardown? tk
    
    
    // API for the source builder - created when initialising proxy
    [<RequireQualifiedAccess>]
    type ProxyBuilderBaseMsg = // rename this to StreamProxyMsgInternal when working tk
        | BuildStreamAPI of ResolvedFunctionTree * Subscriber<(QualifiedTag * Result<MessageHandler<StreamAPI>, FailureReason>)>
        // | HandleBuildResult of RequestID * BuildInfrastructureResult // this is routing straight to proxy broker
        | BuildInfrastructure of string //Interactor address going to be removed per note in interactor.fs? tk
        | TestMsg of string //FunctionBuilderInfo //(unit -> string) //ResolvedFunctionTree //Subscriber<BuildInfrastructureResult> // debug only
        
    
    type StreamAPIBuilderGenerator = Generator2<ResolvedFunctionTree * Subscriber<BuildInfrastructureResult>, ProxyBuilderBaseMsg>

    type StreamRequester = Subscriber<BuilderDoc * Subscriber<BuildInfrastructureResult>> 
    
    // API for deployed proxy - liaises between proxy manager and builder
    // [<RequireQualifiedAccess>]
    // type ProxyMsg = // specialist builder API
    //     | Initialise of QualifiedFunctionName * string // from proxyManager to proxy.  param2 is path to Interactor ? tk
    //     | StreamRequest of BuilderDoc * Subscriber<BuildInfrastructureResult> // BuilderDoc does not look like a recipe doc so perhaps change name tk
    //     | ServiceRequestResult of ClientRef * Result<list<TagAlias>, list<FailureReason>>
    //     | TestMsg of string
    //     | StreamMsg of StreamAPI
    //     // | Other messages such as shutdown?
    
    type InputParameter = 
        | ParamOptions of Options  //should this be here? tk who would consume this?
        | ParamFunctionTree of FunctionTree
        | Break // signals that subsequent inputs should accrue to next stream parameter (equivalent to skip)

    type NamedInputParameter = {ParamName: string; InputParameter: InputParameter}

    type HasHole = bool

    type ParamType = 
        | StreamType of StreamType
        | OptionType    // we will need some kind of description here
        | StreamTypeList of StreamType //dynamic function


    type FunctionSignature = {
        OutType: StreamType; // could a function return a ParamType - i.e. an OptionType?  might make sense in some circs tk
        InTypes: list<ParamType>;
        HasHole: HasHole;
        QFNs: list<QualifiedFunctionName>
    }
    
    // this is the data passed from function to discovery in the case of success following deploy request
    // type BuilderRegistrationPoint = Subscriber<FunctionName * StreamAPIBuilder>

    // this is the data that the deploying function sends to discovery when registering a builder for that function
    type RemoteDeployCallback = Subscriber<QualifiedFunctionName * ActorLocation>

    // this is what deployer sends to discovery when registering
    // root functions and stream sources are deployable
    type RecipeCB = Subscriber<ProviderName * Result<StreamAPIBuilder, FailureReason>>
    
    // this is function registration info
    type FunctionInfo = {
        QName: QualifiedFunctionName;  // will be same as provider if this is the root definition node
        FunctionTree: FunctionTree;  // the function definition
        OutputType: StreamType;
        Dependencies: list<FunctionInfo>;  // functions referred to in this function tree and where to find them if need be. we don't make use of this at the moment tk makeoption?
    }

    
    type ProviderInfo = {
        ProviderName: string;
        ProviderLocation: ActorLocation;
        // Certificate: string; // some identification?
    }

    type ProviderRegistrationInfo = { //may not need this wrapper now that DeployerAPI being phased out
        ProviderInfo: ProviderInfo;     //this may broaden to allow request for service list tk // How to install the builder for this function - only accepts Deploy instruction
        FunctionInfo: FunctionInfo; //make this a list tk
    }

    type ProxyRegistrationInfo = { //may not need this wrapper now that DeployerAPI being phased out
        XProviderInfo: ProviderInfo;     //this may broaden to allow request for service list tk // How to install the builder for this function - only accepts Deploy instruction
        XFunctionInfo: FunctionInfo; //make this a list tk
    }

    
    [<RequireQualifiedAccess>]
    type ProxyRegistrationMsg = 
        // | RegisterBuilders of Subscriber<Result<bool, string>> * list<QualifiedFunctionName * ActorLocation>  // this could also be called DeployResult.  discovery also wants to know about failures
        | RegisterBuilder of QualifiedFunctionName * ActorLocation  // Subscriber is the recipe - or anyone that wans toknow outcome of registration.  this could also be called DeployResult.  discovery also wants to know about failures
        // | RegisterBuilders of list<QualifiedFunctionName * ActorLocation> * Subscriber<Result<bool, string>>  //subscriber is recipe
        // | UnregisterBuilder of QualifiedFunctionName  provider unregisters through its remote proxy
        

        
    // API on provider - this should be broken up into publicand private parts tk
    [<RequireQualifiedAccess>]
    type StreamDeploymentMsg = 
        | DeployStreamBuilder of string * string * QualifiedFunctionName // string * string is sender's system * interActorName
        | BroadcastServices of list<FunctionInfo> 
        | AddDiscoveryCentre of string * ActorLocation
        | RemoveDiscoveryCentre of string
        | TestMsg of string
    
    // API for Stream discovery
    [<RequireQualifiedAccess>]
    type ProviderRegistrationMsg = 
        | RegisterStreamProvider of ProviderRegistrationInfo   //external source of time series data advertises its location
        | UnregisterStreamProvider of ProviderName    //tk this will be qName but will also need to unregister everything.  ProviderName is here equivalent to FunctionName, and these combined are equivalent to FunctionAlias
        | ProxyRegistrationMsg of ProxyRegistrationMsg
        // | DeployProviders of list<QualifiedFunctionName> * RequestID this is being done by provider broker

        // | ParseCommand of string //StreamCommand //callback? I think this will need to move to proxy broker

    [<RequireQualifiedAccess>]
    type StreamDiscoveryResultMsg = // clients looking for streams
        | DiscoveryResult of RequestID * Result<ActorLocation, string>

    [<RequireQualifiedAccess>]
    type StreamRouterMsg =
        | RouteMsg of StreamType2 * obj

    type NoState = NO_STATE
   
    [<RequireQualifiedAccess>]
    type ResourceInit = 
        | Initialise of QualifiedFunctionName * ActorRef

