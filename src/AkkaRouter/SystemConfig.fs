namespace Tweega.AkkaRouter

open Akka.FSharp

module SystemConfig =
    
    // AppActor.fs
    let packItSystemName = "packit-app-system"

    let packItAppActorName = "PackItApp"
    let packItAppPath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" packItAppActorName

    let xamAppSystemConfig =
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
                        port = 9011
                        hostname = localhost
                    }
                }
            }
            """
    
    // UpdateActor.fs
    let updateActorName = "PackItUpdater"
    let updaterPath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" updateActorName

    
    // UpdateActor.fs
    let viewActorName = "PackItViewer"
    let viewerPath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" viewActorName


    //Parser.fs
    let parserActorName = "PackItParser"
    let parserPath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" parserActorName

    
    // TextSource.fs
    let textSourceActorName = "TextSource"
    let textSourcePath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9011/user" textSourceActorName

    

    //Textsourceemulator.fs
    let sttEmulatorActorName = "STTEmulator"
    let sttEmulatorPath = sprintf "akka.tcp://%s%s/%s" packItSystemName "@localhost:9022/user" packItAppActorName

    let sttEmulatorSystemConfig =
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

    //ViewEmulator.fs
    let viewEmulatorSystemName = "emulator-view-system"
    let viewEmulatorActorName = "emulator-view"
    let viewEmulatorPath = sprintf "akka.tcp://%s%s/%s" viewEmulatorSystemName "@localhost:9033/user" viewEmulatorActorName
    
    let viewEmulatorSystemConfig =
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
                        port = 9033
                        hostname = localhost
                    }
                }
            }
            """

    //ViewEmulator.fs
    let viewEmulatorInSystemName = "emulator-view-input-system"
    let viewEmulatorInActorName = "emulator-in-view"
    let viewEmulatorInPath = sprintf "akka.tcp://%s%s/%s" viewEmulatorInSystemName "@localhost:9044/user" viewEmulatorInActorName
    
    let viewEmulatorInSystemConfig =
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
                        port = 9044
                        hostname = localhost
                    }
                }
            }
            """