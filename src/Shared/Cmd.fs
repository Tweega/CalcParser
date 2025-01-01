namespace Tweega.Shared
//Taken from https://github.com/fabulous-dev/Fabulous/blob/d70dfa55992e8fd68e59ff61cb153c3f32a15208/src/Fabulous/Cmd.fs

open System.Threading
open System.Threading.Tasks
open Tweega.Shared.Types


type Effect<'Env> = {
    Description: string;
    Run: Subscriber<'Env>;
    OnError: string -> unit;
}

type Cmd<'Env> = list<Effect<'Env>>

// this is not as controlled as elm as we don't have a single update function
/// Cmd module for creating and manipulating commands
[<RequireQualifiedAccess>]
module Cmd =
    /// Execute the commands using the supplied dispatcher
    let exec (env:'Env) (cmd: Cmd<'Env>) =
        cmd
        |> List.iter(fun (effect) ->
            try
                printfn "Running effect: %s" effect.Description
                effect.Run(env)
            with ex ->
                effect.OnError ex.Message)

    /// None - no commands, also known as `[]`
    let none: Cmd<'Env> = []

    /// Aggregate multiple commands
    let batch (cmds: Cmd<'Env> list) : Cmd<'Env> = List.concat cmds

    /// Command to call the effect
    let ofEffect (effect: Effect<'Env>) : Cmd<'Env> = [ effect ]

    // for more command utilities - eg async, see https://github.com/fabulous-dev/Fabulous/blob/d70dfa55992e8fd68e59ff61cb153c3f32a15208/src/Fabulous/Cmd.fs

