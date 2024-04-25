dotnet publish -c Release -r win-x64 -p:PublishSingleFile=true --self-contained true CryptoBot\CryptoBot.fsproj
copy .\CryptoBot\bin\Release\net8.0\win-x64\publish\CryptoBot.exe .
