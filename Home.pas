unit Home;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  IdBaseComponent, IdComponent, FakeWebsocketServer, WebsocketClient,
  IdTCPConnection, IdTCPClient;

type
  TForm1 = class(TForm)
    clientLogMemo: TMemo;
    command1Button: TButton;
    commandsPanel: TPanel;
    command4Button: TButton;
    command3Button: TButton;
    command2Button: TButton;
    serverLogMemo: TMemo;
    clientPanel: TPanel;
    serverPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure command1ButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure command2ButtonClick(Sender: TObject);
    procedure command3ButtonClick(Sender: TObject);
    procedure command4ButtonClick(Sender: TObject);
  private
    server: TFakeWebsocketServer;
    client: TWebsocketClient;

    procedure logClient(msg: string);
    procedure logServer(msg: string);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}



procedure TForm1.FormCreate(Sender: TObject);
begin
  server := TFakeWebsocketServer.create;
  server.onReceiveCommand := logServer;
  server.onSendResponse := logServer;

  client := TWebsocketClient.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  client.free;
  server.free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  var panelWidth := (width - commandsPanel.Width) / 2;

  clientPanel.Width := panelWidth;
  serverPanel.Width := panelWidth;
end;

procedure TForm1.command1ButtonClick(Sender: TObject);
begin
  var command := 'Command 1';
  logClient('Sending: ' + command);

  client.sendCommand(command, logClient);
end;

procedure TForm1.command2ButtonClick(Sender: TObject);
begin
  var command := 'Command 2';
  logClient('Sending: ' + command);

  client.sendCommand(command, logClient);
end;

procedure TForm1.command3ButtonClick(Sender: TObject);
begin
  var command := 'Command 3';
  logClient('Sending: ' + command);

  client.sendCommand(command, logClient);
end;

procedure TForm1.command4ButtonClick(Sender: TObject);
begin
  var command := 'Command 4';
  logClient('Sending: ' + command);

  client.sendCommand(command, logClient);
end;

procedure TForm1.logClient(msg: string);
begin
  clientLogMemo.Lines.Add(msg);
end;

procedure TForm1.logServer(msg: string);
begin
  serverLogMemo.Lines.Add(msg);
end;

end.
