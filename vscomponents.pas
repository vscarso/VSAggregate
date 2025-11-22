{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit VSComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  VSAggregate, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VSAggregate', @VSAggregate.Register);
end;

initialization
  RegisterPackage('VSComponents', @Register);
end.
