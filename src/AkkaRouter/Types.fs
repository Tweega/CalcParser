namespace Tweega.AkkaRouter

// Note that Akka has a routing component which this may overlap with - rationalise at some point tk
module Types =
    open Akka.Actor
    open Tweega.Shared.XFrameworkTypes
    // generalise this to allow for agents and perhaps other mailbox protocols tk
    // move to somewhere like Tweega.Shared.StreamTypesServer or somewhere all routers can see it

    // to be monadic, AddressResolution would have to be made generic as in Resolution<ActorAddress> At the moment it can only store IActorRef
    [<RequireQualifiedAccess>]
    type AddressResolution = 
        | Resolved of IActorRef
        | Error of FailureReason
        | None

    with 
        member this.bind(f:IActorRef -> AddressResolution) =
            match this with 
            | Resolved actorRef -> f actorRef 
            | x -> this

        member this.onResolved(onSuccess: IActorRef -> unit, onFailure: unit -> unit) =
            match this with 
            | Resolved actorRef ->
                //console.log toConsole( "resolved hzh %s" (actorRef.Path.ToStringWithAddress())
                onSuccess actorRef 
                true
            | _x -> 
                onFailure()
                false
    
        member this.isResolved() =
            let noOp = fun(_x) -> ()
            this.onResolved(noOp,  noOp)
        
    type ActorLocation = {
        AddressResolution: AddressResolution
        ActorPath: string
    }

    [<RequireQualifiedAccess>]
    type ResolvedActorLocation = {
        ActorRef: IActorRef
        ActorPath: string
    }

    type ActorRef = IActorRef
    type ActorSys =  ActorSystem
