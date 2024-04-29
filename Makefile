app=TradingApp
version=net8.0
os=$(shell uname -s)
sources=$(wildcard */*.fs */*/*.fs */*.fsproj)
test_sources=$(wildcard */*/*.py */*/*.fs */*.fsproj)
build_sources=$(wildcard Makefile *.sln build.bat)

ifeq ($(os), Linux)
	runtime=linux-x64
	inp=
	out=.bin
endif

ifeq ($(os), Darwin)
	runtime=osx-x64
	inp=
	out=.bin
endif

ifeq ($(shell uname -o), Cygwin)
	runtime=win-x64
	inp=.exe
	out=.exe
endif


debug: ${sources}
	dotnet publish -c Debug -r ${runtime} -p:PublishSingleFile=true \
		--self-contained true ${app}/${app}.fsproj
	cp ./${app}/bin/Debug/${version}/${runtime}/publish/${app}${inp} ${app}${out}

release: ${sources}
	dotnet publish -c Release -r ${runtime} -p:PublishSingleFile=true \
		--self-contained true ${app}/${app}.fsproj
	cp ./${app}/bin/Release/${version}/${runtime}/publish/${app}${inp} ${app}${out}

print.ps: ${sources}
	a2ps -o $@ --font-size=10 -R --columns=1 $^

print.pdf: print.ps
	ps2pdf -o $@ $^

format: ${sources} ${test_sources} ${build_sources}
	cp ~/Workspace/convert.class .
	java convert $^
	rm convert.class

test:
	rm -rf Test/Temp
	mkdir Test/Temp
	cd Test/ ; dotnet run ; cd ..
	PYTHONPATH="FilterTest/Python" python3 Test/Python/filter_test.py

clean:
	rm -rf */bin */obj *.bin *.exe *.log print.pdf print.ps
