unit WebsocketHandlerTask;

interface

uses
  IdTCPClient, CommandManager, Threading;

type
  TWebsocketHandlerTask = class(TTask, ITask)
   private
     isRunning: boolean;
     client: TIdTCPClient;
     commandManager: TcommandManager;

     procedure createRunnable();
     procedure createWebsocketClient();

  public
    constructor create(commandManager: TCommandManager);

    destructor destroy(); override;

    procedure stop();
  end;

implementation

uses
  System.SysUtils, Classes;

constructor TWebsocketHandlerTask.create;
begin
  inherited create(nil,
                   TNotifyEvent(nil),
                   createRunnable,
                   TThreadPool.Default,
                   nil);

  self.commandManager := commandManager;
end;

destructor TWebsocketHandlerTask.Destroy;
begin
  if (client <> nil) then
  begin
    client.Disconnect;
    client.free;
  end;

  inherited;
end;

procedure TWebsocketHandlerTask.createRunnable;
begin
  isRunning := true;

  createWebsocketClient();

  sleep(20);

  while(isRunning) do
  begin


    var request := commandManager.nextCommand();
    if (request <> '') then
    begin
      client.IOHandler.WriteLn(request);
      var response := client.IOHandler.ReadLn();

      commandManager.notifyCommandListeners(request, response);
    end;

    sleep(20);

  end;

  isRunning := false;
end;

procedure TWebsocketHandlerTask.createWebsocketClient;
begin
   client := TIdTCPClient.create(nil);

  client.Host:='localhost';
  client.Port := 1234;

  client.Connect;
end;


procedure TWebsocketHandlerTask.stop();
begin
  isRunning := false;
end;

end.
