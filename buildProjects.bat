dotnet clean /nowarn:NETSDK1138
dotnet build /nowarn:NETSDK1138 src\XFramework\XFramework.fsproj
dotnet build /nowarn:NETSDK1138 src\AkkaRouter\AkkaRouter.fsproj
dotnet build /nowarn:NETSDK1138 src\StreamSharedClient\StreamSharedClient.fsproj
dotnet build /nowarn:NETSDK1138 src\DiscoveryShared\DiscoveryShared.fsproj
dotnet build /nowarn:NETSDK1138 src\StreamSharedServer\StreamSharedServer.fsproj
dotnet build /nowarn:NETSDK1138 src\StreamInfrastructure\StreamInfrastructure.fsproj
dotnet build /nowarn:NETSDK1138 src\Utils\Utils.fsproj
dotnet build /nowarn:NETSDK1138 src\Recipe\Recipe.fsproj
dotnet build /nowarn:NETSDK1138 src\ProxyShared\ProxyShared.fsproj
@REM dotnet build /nowarn:NETSDK1138 src\Random\Proxy\Proxy.fsproj
dotnet build /nowarn:NETSDK1138 src\Random\Source\Source.fsproj
dotnet build /nowarn:NETSDK1138 src\AF\AF.fsproj
                                                @REM  dotnet build src\StreamWriter\StreamWriter.fsproj // removed
                                                @REM dotnet build src\FileWriter\FileWriter.fsproj
dotnet build /nowarn:NETSDK1138 src\FileWriter\Server\Server.fsproj
dotnet build /nowarn:NETSDK1138 src\Chabbithog\Server\Server.fsproj
dotnet build /nowarn:NETSDK1138 src\Discovery\Discovery.fsproj

dotnet build /nowarn:NETSDK1138 src\App\App.fsproj



dotnet build /nowarn:NETSDK1138 src\Repos\Shared\RepoShared.fsproj
dotnet build /nowarn:NETSDK1138 src\Repos\File\FileSource\Filesource.fsproj


@REM // running order

@REM // start Discovery
@REM // start random 
@REM // start file writer
@REM // ScriptHelper copy.fsx


dotnet build /nowarn:NETSDK1138 src\FileWriter\Server\Server.fsproj

dotnet build /nowarn:NETSDK1138 src\Chabbithog\Server\ChabbithogServer.fsproj
dotnet build /nowarn:NETSDK1138 src\Repos\File\Source\FileSource.fsproj
