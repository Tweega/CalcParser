namespace Tweega.AkkaRouter
open Akka.FSharp
open Akka.Actor
open Tweega.AkkaRouter.Types
open Tweega.Shared.Types
open Tweega.Shared.Utils
open Tweega.Shared.ServerStreamTypes
open Tweega.StreamInfrastructure

[<RequireQualifiedAccess>]
module ActorSystemManager =
    let private config =
        Configuration.parse """
            akka {
                log-config-on-start = on
                stdout-loglevel = DEBUG
                loglevel = DEBUG
                actor {
                    provider = "Akka.Remote.RemoteActorRefProvider, Akka.Remote"
                    serializers {
                        hyperion = "Akka.Serialization.HyperionSerializer, Akka.Serialization.Hyperion"
                    }
                    serialization-bindings {
                        "System.Object" = hyperion
                    }
                }
                akka.actor.serialization-settings.hyperion.cross-platform-package-name-overrides = {
                    netfx = [
                    {
                        fingerprint = "System.Private.CoreLib,%core%",
                        rename-from = "System.Private.CoreLib,%core%",
                        rename-to = "mscorlib,%core%"
                    }]
                    netcore = [
                    {
                        fingerprint = "mscorlib,%core%",
                        rename-from = "mscorlib,%core%",
                        rename-to = "System.Private.CoreLib,%core%"
                    }]
                    net = [
                    {
                        fingerprint = "mscorlib,%core%",
                        rename-from = "mscorlib,%core%",
                        rename-to = "System.Private.CoreLib,%core%"
                    }]
                }
                remote {
                    helios.tcp {
                        transport-class = "Akka.Remote.Transport.Helios.HeliosTcpTransport, Akka.Remote"
                        applied-adapters = []
                        transport-protocol  = tcp
                        port = 9022
                        hostname = localhost
                    }
                }
            }
            """


    let mutable private system: ActorSystem option = None

    let init(systemName: string) =
        if system.IsNone then
            system <- Some(System.create systemName config)
        else
            failwith "ActorSystem is already initialized."

    let shutdown() =
        match system with
        | Some sys -> sys.Terminate() |> Async.AwaitTask |> Async.RunSynchronously
        | None -> failwith "ActorSystem is not initialized."

    let getSystem() =
        match system with
        | Some sys -> sys
        | None -> 
            failwith "ActorSystem is not initialized." // do we want to fail?


// Note that Akka has a routing component which this may overlap with - rationalise at some point tk

    // generalise this to allow for agents and perhaps other mailbox protocols tk
    // move to somewhere like Tweega.Shared.StreamTypesServer or somewhere all routers can see it

    // i think we may want to change the way akka addressing works. Ideally we would not have an overhead of updating address each time we send a message tk
    // used by calcBuilderRouter at the moment

    // should this not be in Types.fs? tk

type ActorLocation<'Msg> = {
    ActorName: string;
    AddressResolution: AddressResolution
    ActorPath: string
}

[<RequireQualifiedAccessAttribute>]
module ActorLocation = 
    

    let makeEmptyLocation(actorName:string) =
        {
            ActorName = actorName;
            ActorPath = "";
            AddressResolution = AddressResolution.None
        }


    let resolveAddress(callerActorSystem: ActorSystem, akkaAddress:ActorLocation<'Msg>, callerID:string)  =
        
        match akkaAddress.AddressResolution with
        | AddressResolution.Resolved _iActorRef ->
            akkaAddress
            
        | _ ->
            try
                let actorRefTask = callerActorSystem.ActorSelection(akkaAddress.ActorPath).ResolveOne(System.TimeSpan.FromSeconds(10.0))
                let iActorRef = actorRefTask |> (Async.AwaitTask >> Async.RunSynchronously)  //or use task computation expression
                let resolution = 
                    iActorRef |> Option.ofObj |> Option.isNone |> fun isNone ->
                        if isNone then
                            let msg = sprintf "Unable to resolve server actor for connection path: %s - caller: %s " akkaAddress.ActorPath callerID
                            AddressResolution.Error (ExecutionError msg)
                        else
                            AddressResolution.Resolved iActorRef
                //console.log toConsole( "qq Resolved address for caller %s : %s" callerID (iActorRef.Path.ToStringWithAddress())
                {akkaAddress with AddressResolution = resolution}
            with 
            | e -> 
                let msg = sprintf "Unable to resolve actor for caller %s : %s :%s" callerID akkaAddress.ActorPath e.Message
                toConsole( sprintf "Error Did not Resolve address %s  (%s)" akkaAddress.ActorPath msg)
                {akkaAddress with AddressResolution = AddressResolution.Error (ExecutionError msg)}


    let tryDispatchMsg(actorSystem: ActorSystem) (location:ActorLocation<'Msg>) (callerID: string) (msg: 'Msg) 
        :Result<option<ActorLocation<'Msg>>,FailureReason>  =

        toConsole( sprintf "yyy Dispatching for %s to %s" callerID location.ActorName)
        match location.AddressResolution with 
        | AddressResolution.Resolved actorRef ->
            //console.log toConsole( "Actor for %s is resolved jj" callerID
            msg |> actorRef.Tell                
            Ok None
        | _x -> 
            // try and resolve from path
            match resolveAddress(actorSystem, location, callerID).AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                msg |> actorRef.Tell
                Ok (Some {location with AddressResolution = AddressResolution.Resolved actorRef;})
            | _x -> 
                let msg = sprintf "Error Unable to resolve actor for %s : %s" location.ActorPath callerID
                Error (OtherReason msg)


    let tryDispatchMsg3(actorSystem: ActorSystem, msg: 'Msg, onFailure: FailureReason -> unit, callerID: string) (location:ActorLocation<'Msg>) =
        // this is intended for use in recipes where we need to inform recipe if we can't deliver a message
        // otherwise the message itself is assumed to carry the callback
        //console.log toConsole( "yyy Dispatching for %s" callerID
        match location.AddressResolution with 
        | AddressResolution.Resolved actorRef ->
            //console.log toConsole( "Actor for %s is resolved jj" callerID   
            msg |> actorRef.Tell
        | _x -> 
            // try and resolve from path
            match resolveAddress(actorSystem, location, callerID).AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                msg |> actorRef.Tell
            | _x -> 
                let msg = sprintf "Error Unable to resolve actor for %s : %s" location.ActorPath callerID
            
                toConsole(msg)
                onFailure(OtherReason msg)
                

    let tryResolve(actorSystem: ActorSystem, callerID: string)  (location:ActorLocation<'Msg>) =
        //console.log toConsole( "oo resolving for %s" callerID
        match location.AddressResolution with 
        | AddressResolution.Resolved actorRef ->
            //console.log toConsole( "Actor for %s is resolved jj" callerID
            Ok location
        | _x -> 
            // try and resolve from path
            match resolveAddress(actorSystem, location, callerID).AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                Ok ({location with AddressResolution = AddressResolution.Resolved actorRef;})
            | _x -> 
                let msg = sprintf "Unable to resolve actor for %s : %s" location.ActorPath callerID
                Error (OtherReason msg)



    let makeBadAd(name, msg: string, path:string) = {
        AddressResolution = AddressResolution.Error (OtherReason msg);
        ActorPath = path;
        ActorName = name
    }

    let makeLocation(name:string, path:string, ar: AddressResolution) = {
        AddressResolution = ar;
        ActorPath = path
        ActorName = name;
    }

    let makeLocationFromPath(name, path:string) = {
        AddressResolution = AddressResolution.None;
        ActorPath = path;
        ActorName = name;
        
    }


    // should we be allowing dirct access to AddressResolutions? tk        
    let resolveAddress2(callerActorSystem: ActorSystem, akkaAddress:ActorLocation<'Msg>, callerID: string)  =
        
        match akkaAddress.AddressResolution with
        | AddressResolution.Resolved _iActorRef ->
            akkaAddress
            
        | _ ->
            try
                let actorRefTask = callerActorSystem.ActorSelection(akkaAddress.ActorPath).ResolveOne(System.TimeSpan.FromSeconds(10.0))
                let iActorRef = actorRefTask |> (Async.AwaitTask >> Async.RunSynchronously)  //or use task computation expression
                let resolution = 
                    iActorRef |> Option.ofObj |> Option.isNone |> fun isNone ->
                        if isNone then
                            let msg = sprintf "Unable to resolve server actor for connection path: %s - caller: %s " akkaAddress.ActorPath callerID
                            AddressResolution.Error (ExecutionError msg)
                        else
                            AddressResolution.Resolved iActorRef
                toConsole(sprintf  "zz Resolved address %s" (iActorRef.Path.ToStringWithAddress()))
                {akkaAddress with AddressResolution = resolution}
            with 
            | e -> 
                let msg = sprintf "Unable to resolve actor for %s :%s" akkaAddress.ActorPath e.Message
                toConsole( sprintf "Error zzz Did not Resolve address %s  (%s)" akkaAddress.ActorPath callerID)
                {akkaAddress with AddressResolution = AddressResolution.Error (ExecutionError msg)}

    let createAkkaMailbox<'Msg, 'State>
        (actorSystem, 
        actorName: string, 
        handleMsg: 'Msg -> 'State -> 'State * Tweega.Shared.Cmd<Actor<'Msg>>, 
        initialState: 'State) : IActorRef =
        //console.log toConsole( "in AkkaRouter createAkkaMailbox, creating mailbox : %s\n" actorName
                
        spawn actorSystem actorName <| fun (mailbox: Actor<'Msg>) ->
        let rec loop (state: 'State) = actor {
            let! (msg:'Msg) = mailbox.Receive()
            
            let (newState, cmd:Tweega.Shared.Cmd<Actor<'Msg>>) = handleMsg msg state
            
            // process any side effects, which may result in messages to this actor
            Tweega.Shared.Cmd.exec mailbox cmd
            
            return! loop newState
        }
        loop initialState

    let createAkkaMailboxInDefaultSystem<'Msg, 'State>
        (actorName: string, 
        handleMsg: 'Msg -> 'State -> 'State * Tweega.Shared.Cmd<Actor<'Msg>>, 
        initialState: 'State) =
        
        //(actorName: string, handleMsg: Actor<'MsgType> -> 'MsgType -> 'State -> 'State, initialState: 'State) : IActorRef =
        let actorSystem = ActorSystemManager.getSystem()
        let iActorRef = createAkkaMailbox(actorSystem, actorName, handleMsg, initialState)
        let location = AddressResolution.Resolved iActorRef
        let actorPath = "" //sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" packItAppActorName
        let location = makeLocation(actorName, actorPath, location)
        let dispatcher = tryDispatchMsg actorSystem location actorName
        
        fun (msg: 'Msg) ->
            let dispatchResult: Result<option<ActorLocation<'Msg>>, FailureReason> = 
                msg |> dispatcher
            dispatchResult
            

    // we may not end up using this - orchestrator buffers won't be mail boxes
    let createBufferBoxInDefaultSystem<'bufIn, 'bufInA, 'bufOut, 'bufState>
        ( bufName: string, 
        initialBufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =

        let handleMsg = 
            fun
                (msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) 
                (bufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) ->
                let buffState = handleBufferMessage msg bufferState
                (buffState, Tweega.Shared.Cmd.none)

        let actorSystem = ActorSystemManager.getSystem()
        let iActorRef = createAkkaMailbox(actorSystem, bufName, handleMsg, initialBufferState)
        let address = AddressResolution.Resolved iActorRef
        let actorPath = "" //sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" packItAppActorName
        let loc = makeLocation(bufName, actorPath, address) // should we be using actor locations for buffers?
        let dispatcher = tryDispatchMsg actorSystem loc bufName
        // the idea behind actor locations was that they might bu built on to create self-healing actors
        // if this actor falls over we won't have access to the location to change it.
        // we only get a new location if the path can be resolved.  If it can't then we need 
        // to replace failure reason with something that can help resolve the situation - a recipe of some kind tk
        // perhaps when we create the buff box we can pass in a disaster recivery handler which the actor location 
        // would call and we would then call into a recipe that had been set up, saying where to notify when things running again
        // the dispatcher would have to be recreated as it is bound to an immutable loc variable

        fun (msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) ->
            let dispatchResult: Result<option<ActorLocation<BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>>>, FailureReason> = 
                msg |> dispatcher
            dispatchResult
            

    // for actors created remotely
    // StreamProxyMsgInternal<'T>>
    let getAkkaMailbox<'MsgType, 'State>(handleMsg: Actor<'MsgType> -> 'MsgType -> 'State -> 'State, initialState: 'State) =
        fun (mailbox: Actor<'MsgType>) ->
            let rec loop(state: 'State): Cont<'MsgType, 'State> =
                actor {
                    let! (msg:'MsgType) = mailbox.Receive()

                    let newState = handleMsg mailbox msg state

                    return! loop newState
                }
            loop(initialState)


    let tryParseAddress addressPath = 
        try
            let jj = Address.Parse addressPath    
            Some jj
        with 
            | _e -> None

    let remoteDeploy systemPath = 
        let address = 
            match  tryParseAddress systemPath with
            | None -> 
                let msg = sprintf "ActorPath address cannot be parsed: %s" systemPath 
                failwith msg
            | Some a -> a
        Deploy(RemoteScope(address))

// see if it is possible to add some restriction to the surface area of target actor - which would make actor location generic on this sub-message type tk
