id=ScanRat
version=0.5.0.0
package=${id}.${version}.nupkg

.PHONY: pushnuget
pushnuget: nuget
	cd ScanRat && nuget push ${package}

.PHONY: nuget
nuget: build
	cd ScanRat && nuget pack -Version ${version} ${id}.fsproj

MSB=msbuild.exe /m /verbosity:m /nologo

.PHONY: build
build:
	${MSB} ScanRat.sln /p:Configuration=Release /t:"ScanRat:Rebuild"

