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

    let get() =
        match system with
        | Some sys -> sys
        | None -> 
            failwith "ActorSystem is not initialized."


// Note that Akka has a routing component which this may overlap with - rationalise at some point tk
module Mailbox =

    // generalise this to allow for agents and perhaps other mailbox protocols tk
    // move to somewhere like Tweega.Shared.StreamTypesServer or somewhere all routers can see it

    // i think we may want to change the way akka addressing works. Ideally we would not have an overhead of updating address each time we send a message tk
    // used by calcBuilderRouter at the moment

    // should this not be in Types.fs? tk

    type ActorLocation<'Msg> = {
        AddressResolution: AddressResolution
        ActorPath: string
    }
    with
        //instance members defined in Tweega.AkkaRouter.Mailbox  - why keep these separate?
        static member EmptyLocation() =
            {
                ActorPath = "";
                AddressResolution = AddressResolution.None
            }

        member this.tryDispatchMsgz(actorSystem: ActorSystem, msg: 'Msg) =
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                msg |> actorRef.Tell
                Ok None
            | _x -> 
                // try and resolve from path
                match ActorLocation.resolveAddress(actorSystem, this, "tryDispatchMsgz should not be in use").AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    msg |> actorRef.Tell
                    Ok (Some {ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Unable to resolve actor for %s" this.ActorPath
                    Error (OtherReason msg)

        member this.tryDispatchMsg2(actorSystem: ActorSystem, msg: 'Msg, callerID: string) =
            toConsole( sprintf "yyy Dispatching for %s" callerID)
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID
                msg |> actorRef.Tell                
                Ok None
            | _x -> 
                // try and resolve from path
                match ActorLocation.resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    msg |> actorRef.Tell
                    Ok (Some {ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Error Unable to resolve actor for %s : %s" this.ActorPath callerID
                    Error (OtherReason msg)


        member this.tryDispatchMsg3(actorSystem: ActorSystem, msg: 'Msg, onFailure: FailureReason -> unit, callerID: string) =
            // this is intended for use in recipes where we need to inform recipe if we can't deliver a message
            // otherwise the message itself is assumed to carry the callback
            //console.log toConsole( "yyy Dispatching for %s" callerID
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID   
                msg |> actorRef.Tell
            | _x -> 
                // try and resolve from path
                match ActorLocation.resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    msg |> actorRef.Tell
                | _x -> 
                    let msg = sprintf "Error Unable to resolve actor for %s : %s" this.ActorPath callerID
                    

                    toConsole(msg)
                    onFailure(OtherReason msg)
                    

        member this.tryResolve(actorSystem: ActorSystem, callerID: string) =
            //console.log toConsole( "oo resolving for %s" callerID
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID
                Ok this
            | _x -> 
                // try and resolve from path
                match ActorLocation.resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    Ok ({ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Unable to resolve actor for %s : %s" this.ActorPath callerID
                    Error (OtherReason msg)


        static member postMessage<'Msg>(iActorRef: IActorRef) (msg: 'Msg) =
            // wrap this in a try tk?
            iActorRef.Tell msg

        static member  makeBadAd(msg: string, path:string) = {
            AddressResolution = AddressResolution.Error (OtherReason msg);
            ActorPath = path;
        }

        static member  makeLocation(path:string, ar: AddressResolution) = {
            AddressResolution = ar;
            ActorPath = path;
        }

        static member  makeLocationFromPath(path:string) = {
            AddressResolution = AddressResolution.None;
            ActorPath = path;
        }

        static member  resolveAddress(callerActorSystem: ActorSystem, akkaAddress:ActorLocation<'Msg>, callerID:string)  =
            
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


        static member resolveAddress2(callerActorSystem: ActorSystem, akkaAddress:ActorLocation<'Msg>, callerID: string)  =
            
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
        initialState: 'State) : ActorLocation<'Msg> =
        
        //(actorName: string, handleMsg: Actor<'MsgType> -> 'MsgType -> 'State -> 'State, initialState: 'State) : IActorRef =
        let actorSystem = ActorSystemManager.get()
        let iActorRef = createAkkaMailbox(actorSystem, actorName, handleMsg, initialState)
        let location = AddressResolution.Resolved iActorRef
        let actorPath = "" //sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" packItAppActorName
        ActorLocation.makeLocation<'Msg>(actorPath, location)
         
    let createBufferBoxInDefaultSystem<'bufIn, 'bufInA, 'bufOut, 'bufState>
        ( bufName: string, 
        initialBufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) =

        let handleMsg = 
            fun
                (msg: BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>) 
                (bufferState: BufferState<'bufIn, 'bufInA, 'bufOut, 'bufState>) ->
                let gg = handleBufferMessage msg bufferState
                (gg, Tweega.Shared.Cmd.none)

        let actorSystem = ActorSystemManager.get()
        let iActorRef = createAkkaMailbox(actorSystem, bufName, handleMsg, initialBufferState)
        let location = AddressResolution.Resolved iActorRef
        let actorPath = "" //sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" packItAppActorName
        ActorLocation.makeLocation<BufferMsg<'bufIn, 'bufInA, 'bufOut, 'bufState>>(actorPath, location)
        

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


    // type PackItAppSys = 
    //     static let PackItAppSysh = System.create SystemConfig.packItSystemName SystemConfig.xamAppSystemConfig

    type PackItAppSys private() =
        // let jj = ActorSystem()

        let pckItAppSystem = System.create SystemConfig.packItSystemName SystemConfig.xamAppSystemConfig

        static let instance = new PackItAppSys()
        static member Instance = instance
        member __.ActorSystem
            with get() = pckItAppSystem
        
