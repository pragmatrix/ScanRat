id=ScanRat
version=0.1.0.0
package=ScanRat/${id}.${version}.nupkg

.PHONY: pushnuget
pushnuget: nuget
	nuget push ${package}

.PHONY: nuget
nuget:
	cd ScanRat && nuget pack -Properties "id=${id};version=${version}" ScanRat.fsproj

