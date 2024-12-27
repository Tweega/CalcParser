namespace Tweega.AkkaRouter

// Note that Akka has a routing component which this may overlap with - rationalise at some point tk
module Utils =
    open Akka.FSharp
    open Akka.Actor
    open Tweega.AkkaRouter.Types
    open Tweega.Shared.XFrameworkTypes
    open Tweega.Utils

    // generalise this to allow for agents and perhaps other mailbox protocols tk
    // move to somewhere like Tweega.Shared.StreamTypesServer or somewhere all routers can see it

    // i think we may want to change the way akka addressing works. Ideally we would not have an overhead of updating address each time we send a message tk
    // used by calcBuilderRouter at the moment
    let postMessage<'Msg>(iActorRef: IActorRef) (msg: 'Msg) =
        // wrap this in a try tk?
        iActorRef.Tell msg

    let makeBadAd(msg: string, path:string) = {
        AddressResolution = AddressResolution.Error (OtherReason msg);
        ActorPath = path;
    }

    let makeLocation(path:string, ar: AddressResolution) = {
        AddressResolution = ar;
        ActorPath = path;
    }

    let makeEmptyLocation(path:string) = {
        AddressResolution = AddressResolution.None;
        ActorPath = path;
    }

    let resolveAddress(callerActorSystem: ActorSystem, akkaAddress:ActorLocation, callerID:string)  =
        
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


    let resolveAddress2(callerActorSystem: ActorSystem, akkaAddress:ActorLocation, callerID: string)  =
        
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
    // 'Key will be StringType for calc builder router
    // let forwardAkkaMessage = fun (actorSystem: ActorSystem) (connectionPath: string) (msg: 'Msg) ((key: 'Key, map: Map<'Key, AkkaAddress>)) (caller: string) ->

    //     match map.TryFind key with
    //     | Some akkaAddress ->
    //         postMessage<'Msg> actorSystem caller akkaAddress msg
    //     | None ->
    //         let msg = sprintf "Unable to find message handler from map in forwardMessage: %s, %A" caller key   //how to handle streamwriter not found? tk
    //         (Error (akkaAddress, msg))


    let createAkkaMailbox<'MsgType, 'State>(actorSystem, actorName: string, handleMsg: Actor<'MsgType> -> 'MsgType -> 'State -> 'State, initialState: 'State) : IActorRef =
        //console.log toConsole( "in AkkaRouter createAkkaMailbox, creating mailbox : %s\n" actorName
        spawn actorSystem actorName <| fun (mailbox: Actor<'MsgType>) ->
        let rec loop (state: 'State) = actor {
            let! (msg:'MsgType) = mailbox.Receive()

            let newState = handleMsg mailbox msg state
            return! loop newState
        }
        loop initialState

    // for actors created remotely
    //StreamProxyMsgInternal<'T>>
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
    type ActorLocation with
        member this.tryDispatchMsgz(actorSystem: ActorSystem, onResolve: IActorRef -> unit) =
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                onResolve actorRef 
                Ok None
            | _x -> 
                // try and resolve from path
                match resolveAddress(actorSystem, this, "tryDispatchMsgz should not be in use").AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    onResolve actorRef 
                    Ok (Some {ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Unable to resolve actor for %s" this.ActorPath
                    Error (OtherReason msg)

        member this.tryDispatchMsg2(actorSystem: ActorSystem, onResolve: IActorRef -> unit, callerID: string) =
            //console.log toConsole( "yyy Dispatching for %s" callerID
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID
                onResolve actorRef 
                Ok None
            | _x -> 
                // try and resolve from path
                match resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    onResolve actorRef 
                    Ok (Some {ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Error Unable to resolve actor for %s : %s" this.ActorPath callerID
                    Error (OtherReason msg)


        member this.tryDispatchMsg3(actorSystem: ActorSystem, onResolve: IActorRef -> unit, onFailure: FailureReason -> unit, callerID: string) =
            // this is intended for use in recipes where we need to inform recipe if we can't deliver a message
            //console.log toConsole( "yyy Dispatching for %s" callerID
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID            
                onResolve actorRef 
            | _x -> 
                // try and resolve from path
                match resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    onResolve actorRef 
                | _x -> 
                    let msg = sprintf "Error Unable to resolve actor for %s : %s" this.ActorPath callerID
                    

                    toConsole(msg)
                    onFailure(OtherReason msg)
                    

        member this.tryResolve(actorSystem: ActorSystem, callerID: string) =
            //console.log toConsole( "oo resolving for %s" callerID
            match this.AddressResolution with 
            | AddressResolution.Resolved actorRef ->
                //console.log toConsole( "Actor for %s is resolved jj" callerID
                Ok None
            | _x -> 
                // try and resolve from path
                match resolveAddress(actorSystem, this, callerID).AddressResolution with 
                | AddressResolution.Resolved actorRef ->
                    Ok (Some {ActorPath = this.ActorPath; AddressResolution = AddressResolution.Resolved actorRef;})
                | _x -> 
                    let msg = sprintf "Unable to resolve actor for %s : %s" this.ActorPath callerID
                    Error (OtherReason msg)

