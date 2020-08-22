id=ScanRat

.PHONY: publish
publish: package
	dotnet nuget push --api-key ${NUGETAPIKEY} --source https://api.nuget.org/v3/index.json tmp/*.nupkg 

.PHONY: package
package: rebuild
	mkdir -p tmp
	rm -f tmp/*.nupkg
	dotnet clean -c Release
	cd ScanRat && dotnet pack -c Release -o ../tmp

.PHONY: rebuild
rebuild:
	dotnet clean -c Release
	dotnet build -c Release ScanRat.sln

