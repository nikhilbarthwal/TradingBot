dotnet publish -c Release -r win-x64 -p:PublishSingleFile=true --self-contained true SampleBot\SampleBot.fsproj
copy .\SampleBot\bin\Release\net8.0\win-x64\publish\SampleBot.exe .
