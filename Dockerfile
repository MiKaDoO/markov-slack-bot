FROM microsoft/dotnet:2.0-sdk

WORKDIR /src

COPY ./src .

RUN dotnet restore

CMD ["dotnet", "run"]