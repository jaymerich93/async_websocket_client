unit WebsocketClient;

interface

uses
  Threading, CommandManager, WebsocketHandlerTask;

type
  TWebsocketClient = class(TObject)
    private
      websocketHandlerTask: TWebsocketHandlerTask;
      commandManager: TCommandManager;

    public
      constructor create();

      destructor destroy(); override;

      procedure sendCommand(command: string; onResponseEvent: TOnResponseEvent);
  end;


implementation

constructor TWebsocketClient.create;
begin
  commandManager := TCommandManager.create;

  websocketHandlerTask := TWebsocketHandlerTask.create(commandManager);
  ITask(websocketHandlerTask).Start;
end;

destructor TWebsocketClient.destroy;
begin
  websocketHandlerTask.stop;
  websocketHandlerTask := nil;

  commandManager.Destroy;
  commandManager := nil;

  inherited;
end;

procedure TWebsocketClient.sendCommand(command: string; onResponseEvent:
  TOnResponseEvent);
begin
  commandManager.queueCommand(command, onResponseEvent);
end;

end.

