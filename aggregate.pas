{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit aggregate;

{$warn 5023 off : no warning about unused units}
interface

uses
  VSAggregate, VSAggregateEditors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VSAggregateEditors', @VSAggregateEditors.Register);
end;

initialization
  RegisterPackage('aggregate', @Register);
end.
