program async_websocket_client;

uses
  System.StartUpCopy,
  FMX.Forms,
  Home in 'Home.pas' {Form1},
  CommandManager in 'client\CommandManager.pas',
  WebsocketClient in 'client\WebsocketClient.pas',
  WebsocketHandlerTask in 'client\WebsocketHandlerTask.pas',
  FakeWebsocketServer in 'server\FakeWebsocketServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
