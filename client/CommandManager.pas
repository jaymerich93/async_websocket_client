unit CommandManager;

interface

uses
  Generics.Collections, Classes, SysUtils, SyncObjs;

type
  TOnResponseEvent = TProc<string>;

  TCommandManager = class(TObject)
  private
    commandQueue: TList<String>;
    commandQueueSet: THashSet<String>;
    listenersMap: TDictionary<String, TList<TOnResponseEvent>>;
    criticalSection: TCriticalSection;

    procedure addCommandListener(command: String; listener: TOnResponseEvent);

  public
    constructor create();

    destructor destroy(); override;

    procedure queueCommand(command: String; listener: TOnResponseEvent);
    procedure notifyCommandListeners(command: string; response: string);

    function nextCommand(): String;
  end;

implementation

constructor TCommandManager.create;
begin
  inherited;

  commandQueue := TList<String>.create();
  commandQueueSet := THashSet<String>.create();
  listenersMap := TDictionary<String, TList<TOnResponseEvent>>.create();
  criticalSection := TCriticalSection.Create;
end;

destructor TCommandManager.Destroy;
begin
  commandQueue.free;
  commandQueueSet.free;
  listenersMap.free;
  criticalSection.free;

  inherited;
end;

procedure TCommandManager.queueCommand(command: string; listener:
  TOnResponseEvent);
begin
  criticalSection.Enter;
  try
    addCommandListener(command, listener);

    if not commandQueueSet.Contains(command) then
    begin
      commandQueueSet.Add(command);
      commandQueue.Add(command);
    end;
  finally
    criticalSection.Leave;
  end;
end;

procedure TCommandManager.addCommandListener(command: string; listener:
  TOnResponseEvent);
begin
  if not listenersMap.ContainsKey(command) then
  begin
    var newListeners := TList<TOnResponseEvent>.Create();
    listenersMap.Add(command, newListeners);
  end;

  listenersMap[command].Add(listener);
end;

procedure TCommandManager.notifyCommandListeners(command: string; response:
  String);
begin
  var listeners: TList<TOnResponseEvent>;

  criticalSection.Enter;
  try
    if (commandQueue.First = command) then
      commandQueue.ExtractAt(0);
    commandQueueSet.Remove(command);

    listenersMap.TryGetValue(command, listeners);
    if (listeners <> nil) then
      listenersMap.Remove(command);
  finally
    criticalSection.leave;
  end;

  if (listeners <> nil) then
  begin
    for var listener in listeners do
      listener(response);
  end;
end;

function TCommandManager.nextCommand: string;
begin
  criticalSection.Enter;
  try
    var numPendingCommands := commandQueue.Count;
    if numPendingCommands > 0 then
      result := commandQueue.First;
  finally
    criticalSection.Leave;
  end;
end;

end.
