unit udemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  mePositionGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    mePositionGrid1: TmePositionGrid;
    procedure FormCreate(Sender: TObject);
    procedure mePositionGrid1PositionChanged(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
   mePositionGrid1PositionChanged(Self);
end;

procedure TForm1.mePositionGrid1PositionChanged(Sender: TObject);
begin
    // Handler for position change
  case mePositionGrid1.SelectedPosition of
    gpTopLeft: Label1.Caption := 'Selected: Top Left';
    gpTopCenter: Label1.Caption := 'Selected: Top Center';
    gpTopRight: Label1.Caption := 'Selected: Top Right';
    gpCenterLeft: Label1.Caption := 'Selected: Center Left';
    gpCenter: Label1.Caption := 'Selected: Center';
    gpCenterRight: Label1.Caption := 'Selected: Center Right';
    gpBottomLeft: Label1.Caption := 'Selected: Bottom Left';
    gpBottomCenter: Label1.Caption := 'Selected: Bottom Center';
    gpBottomRight: Label1.Caption := 'Selected: Bottom Right';
  end;
end;

end.

