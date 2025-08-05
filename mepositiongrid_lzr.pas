{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mePositionGrid_lzr;

{$warn 5023 off : no warning about unused units}
interface

uses
  mePositionGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('mePositionGrid', @mePositionGrid.Register);
end;

initialization
  RegisterPackage('mePositionGrid_lzr', @Register);
end.
