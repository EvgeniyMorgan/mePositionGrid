unit mePositionGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Buttons, Math, LResources;

type
  TGridPosition = (
    gpTopLeft, gpTopCenter, gpTopRight,
    gpCenterLeft, gpCenter, gpCenterRight,
    gpBottomLeft, gpBottomCenter, gpBottomRight
  );

  TmePositionGrid = class(TCustomPanel)
  private
    FButtons: array[0..8] of TSpeedButton;
    FSelectedPosition: TGridPosition;
    FOnPositionChanged: TNotifyEvent;
    FSelectionBitmap: TBitmap;
    FUnselectionBitmap: TBitmap;
    FSpacing: Integer;
    FButtonHints: TStrings;
    FButtonSize: Integer;
    FStartPosition: TGridPosition;
    FDesignMode: Boolean;

    procedure ButtonClick(Sender: TObject);
    procedure SetSelectedPosition(AValue: TGridPosition);
    procedure SetSelectionBitmap(AValue: TBitmap);
    procedure SetUnselectionBitmap(AValue: TBitmap);
    procedure SetSpacing(AValue: Integer);
    procedure SetButtonHints(AValue: TStrings);
    procedure SetButtonSize(AValue: Integer);
    procedure SetStartPosition(AValue: TGridPosition);
    procedure UpdateButtons;
    procedure BitmapsChanged(Sender: TObject);
    procedure CenterGlyph(Button: TSpeedButton);
    procedure UpdateComponentSize;
    procedure Paint; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  protected
    procedure Loaded; override;
    procedure SetDesignMode(Value: Boolean);
    function GetDesignMode: Boolean;
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedPosition: TGridPosition read FSelectedPosition write SetSelectedPosition;
    property DesignMode: Boolean read GetDesignMode write SetDesignMode;
  published
    property SelectionBitmap: TBitmap read FSelectionBitmap write SetSelectionBitmap;
    property UnselectionBitmap: TBitmap read FUnselectionBitmap write SetUnselectionBitmap;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 40;
    property StartPosition: TGridPosition read FStartPosition write SetStartPosition default gpCenter;
    property ButtonHints: TStrings read FButtonHints write SetButtonHints;
    property OnPositionChanged: TNotifyEvent read FOnPositionChanged write FOnPositionChanged;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Morgunov', [TmePositionGrid]);
end;

constructor TmePositionGrid.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
  FSpacing := 2;
  FButtonSize := 40;
  FStartPosition := gpCenter;
  FSelectedPosition := FStartPosition;
  FDesignMode := csDesigning in ComponentState;

  FSelectionBitmap := TBitmap.Create;
  FSelectionBitmap.OnChange := @BitmapsChanged;
  FUnselectionBitmap := TBitmap.Create;
  FUnselectionBitmap.OnChange := @BitmapsChanged;

  FButtonHints := TStringList.Create;
  with TStringList(FButtonHints) do
  begin
    Add('Top Left');
    Add('Top Center');
    Add('Top Right');
    Add('Center Left');
    Add('Center');
    Add('Center Right');
    Add('Bottom Left');
    Add('Bottom Center');
    Add('Bottom Right');
  end;

  for i := 0 to 8 do
  begin
    FButtons[i] := TSpeedButton.Create(Self);
    with FButtons[i] do
    begin
      Parent := Self;
      GroupIndex := 1;
      AllowAllUp := False;
      Tag := i;
      OnClick := @ButtonClick;
      Flat := True;
      ShowHint := True;
      AutoSize := False;
      Margin := -1;
      Spacing := 0;
    end;
  end;

  UpdateComponentSize;
  UpdateButtons;
end;

destructor TmePositionGrid.Destroy;
begin
  FSelectionBitmap.Free;
  FUnselectionBitmap.Free;
  FButtonHints.Free;
  inherited Destroy;
end;

procedure TmePositionGrid.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, FButtonSize * 3 + FSpacing * 4, FButtonSize * 3 + FSpacing * 4);
end;

procedure TmePositionGrid.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(1, 1);
end;

procedure TmePositionGrid.Loaded;
begin
  inherited Loaded;
  FSelectedPosition := FStartPosition;
  UpdateComponentSize;
  UpdateButtons;
end;

procedure TmePositionGrid.Paint;
var
  i: Integer;
begin
  inherited Paint;
  if FDesignMode then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
    for i := 0 to 8 do
    begin
      if Assigned(FButtons[i]) then
      begin
        Canvas.Pen.Style := psDot;
        Canvas.Pen.Color := clGray;
        Canvas.Rectangle(FButtons[i].BoundsRect);
      end;
    end;
  end;
end;

procedure TmePositionGrid.SetDesignMode(Value: Boolean);
begin
  if FDesignMode <> Value then
  begin
    FDesignMode := Value;
    Invalidate;
  end;
end;

function TmePositionGrid.GetDesignMode: Boolean;
begin
  Result := FDesignMode;
end;

procedure TmePositionGrid.SetSelectedPosition(AValue: TGridPosition);
begin
  if FSelectedPosition = AValue then Exit;
  FSelectedPosition := AValue;
  UpdateButtons;
  if Assigned(FOnPositionChanged) then
    FOnPositionChanged(Self);
end;

procedure TmePositionGrid.SetSelectionBitmap(AValue: TBitmap);
begin
  FSelectionBitmap.Assign(AValue);
end;

procedure TmePositionGrid.SetUnselectionBitmap(AValue: TBitmap);
begin
  FUnselectionBitmap.Assign(AValue);
end;

procedure TmePositionGrid.SetSpacing(AValue: Integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  UpdateComponentSize;
  UpdateButtons;
end;

procedure TmePositionGrid.SetButtonHints(AValue: TStrings);
begin
  if FButtonHints.Text = AValue.Text then Exit;
  FButtonHints.Assign(AValue);
  UpdateButtons;
end;

procedure TmePositionGrid.SetButtonSize(AValue: Integer);
begin
  if (FButtonSize = AValue) or (AValue < 10) then Exit;
  FButtonSize := AValue;
  UpdateComponentSize;
  UpdateButtons;
end;

procedure TmePositionGrid.SetStartPosition(AValue: TGridPosition);
begin
  if FStartPosition = AValue then Exit;
  FStartPosition := AValue;
  FSelectedPosition := AValue;
  if not (csLoading in ComponentState) then
  begin
    UpdateButtons;
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);
  end;
end;

procedure TmePositionGrid.BitmapsChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TmePositionGrid.CenterGlyph(Button: TSpeedButton);
var
  NewLeft, NewTop: Integer;
begin
  if not Assigned(Button.Glyph) or Button.Glyph.Empty then Exit;
  NewLeft := (Button.Width - Button.Glyph.Width) div 2;
  NewTop := (Button.Height - Button.Glyph.Height) div 2;
  Button.Glyph.Transparent := True;
  Button.Glyph.TransparentColor := clFuchsia;
  Button.Canvas.Draw(NewLeft, NewTop, Button.Glyph);
end;

procedure TmePositionGrid.UpdateComponentSize;
begin
  SetBounds(Left, Top, FButtonSize * 3 + FSpacing * 4, FButtonSize * 3 + FSpacing * 4);
end;

procedure TmePositionGrid.UpdateButtons;
var
  i, row, col: Integer;
begin
  if csLoading in ComponentState then Exit;

  for i := 0 to 8 do
  begin
    if not Assigned(FButtons[i]) then Continue;

    row := i div 3;
    col := i mod 3;

    FButtons[i].SetBounds(
      col * (FButtonSize + FSpacing) + FSpacing,
      row * (FButtonSize + FSpacing) + FSpacing,
      FButtonSize,
      FButtonSize
    );

    FButtons[i].Down := (i = Ord(FSelectedPosition));

    if FButtons[i].Down then
      FButtons[i].Glyph := FSelectionBitmap
    else
      FButtons[i].Glyph := FUnselectionBitmap;

    CenterGlyph(FButtons[i]);

    if (i < FButtonHints.Count) and (FButtonHints[i] <> '') then
      FButtons[i].Hint := FButtonHints[i]
    else
      case i of
        0: FButtons[i].Hint := 'Top Left';
        1: FButtons[i].Hint := 'Top Center';
        2: FButtons[i].Hint := 'Top Right';
        3: FButtons[i].Hint := 'Center Left';
        4: FButtons[i].Hint := 'Center';
        5: FButtons[i].Hint := 'Center Right';
        6: FButtons[i].Hint := 'Bottom Left';
        7: FButtons[i].Hint := 'Bottom Center';
        8: FButtons[i].Hint := 'Bottom Right';
      end;
  end;

  if FDesignMode then
    Invalidate;
end;

procedure TmePositionGrid.ButtonClick(Sender: TObject);
begin
  if not (Sender is TSpeedButton) then Exit;
  SelectedPosition := TGridPosition((Sender as TSpeedButton).Tag);
end;

initialization
  {$I mePositionGrid.lrs}

end.
