dotnet clean
dotnet build src\XFramework\XFramework.fsproj
dotnet build src\Utils\Utils.fsproj
dotnet build src\AkkaRouter\AkkaRouter.fsproj
dotnet build src\StreamSharedClient\StreamSharedClient.fsproj
dotnet build src\DiscoveryShared\DiscoveryShared.fsproj
dotnet build src\StreamSharedServer\StreamSharedServer.fsproj
dotnet build src\StreamInfrastructure\StreamInfrastructure.fsproj
dotnet build src\Recipe\Recipe.fsproj
dotnet build src\ProxyShared\ProxyShared.fsproj
@REM dotnet build src\Random\Proxy\Proxy.fsproj
dotnet build src\Random\Source\Source.fsproj
dotnet build src\AF\AF.fsproj
                                                @REM  dotnet build src\StreamWriter\StreamWriter.fsproj // removed
                                                @REM dotnet build src\FileWriter\FileWriter.fsproj
dotnet build src\FileWriter\Server\Server.fsproj
dotnet build src\Chabbithog\Server\Server.fsproj
dotnet build src\Discovery\Discovery.fsproj

dotnet build src\App\App.fsproj



dotnet build src\Repos\Shared\RepoShared.fsproj
dotnet build src\Repos\File\FileSource\Filesource.fsproj


@REM // running order

@REM // start Discovery
@REM // start random 
@REM // start file writer
@REM // ScriptHelper copy.fsx


dotnet build src\FileWriter\Server\Server.fsproj

dotnet build src\Chabbithog\Server\ChabbithogServer.fsproj
dotnet build src\Repos\File\Source\FileSource.fsproj
