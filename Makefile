id=ScanRat
version=0.5.0.0
package=${id}.${version}.nupkg

.PHONY: pushnuget
pushnuget: nuget
	cd ScanRat && nuget push ${package}

.PHONY: nuget
nuget:
	cd ScanRat && nuget pack -Version ${version} ${id}.fsproj

