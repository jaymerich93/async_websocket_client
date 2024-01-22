unit FakeWebsocketServer;

interface

uses
  IdTCPServer, IdContext;

type
  TServerEvent = procedure(msg: String) of object;

  TFakeWebsocketServer = class(TObject)
    private
      server: TIdTCPServer;
      fOnReceiveCommand: TServerEvent;
      fOnSendResponse: TServerEvent;

      procedure createServer();
      procedure onExecute(AContext: TIdContext);

    public
      constructor create();
      destructor destroy(); override;

      property onReceiveCommand: TServerEvent write fOnReceiveCommand;
      property onSendResponse: TServerEvent write fOnSendResponse;
  end;

implementation

uses
  System.SysUtils;

constructor TFakeWebsocketServer.create();
begin
  createServer();
end;

destructor TFakeWebsocketServer.Destroy;
begin
  server.Active := false;
  server.Destroy;

  inherited;
end;

procedure TFakeWebsocketServer.createServer;
begin
  server := TIdTCPServer.Create();

  server.DefaultPort := 1234;
  server.OnExecute := onExecute;

  server.Active := true;
end;

procedure TFakeWebsocketServer.onExecute(AContext: TIdContext);
begin
  var messageReceived := AContext.Connection.IOHandler.ReadLn;
  fOnReceiveCommand(messageReceived);

  sleep(3000);

  if (AContext.Connection.Connected) then
  begin
    var response := 'Response: ' + messageReceived;
    AContext.Connection.IOHandler.WriteLn('Response: ' + messageReceived);
    fOnSendResponse(response);
  end;
end;

end.
