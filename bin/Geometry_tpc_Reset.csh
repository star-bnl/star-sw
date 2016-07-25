set d = "StarDb/Geometry/tpc/";
foreach table (TpcHalfPosition TpcInnerSectorPositionB TpcOuterSectorPositionB tpcPadPlanes.devTB TpcPosition TpcSuperSectorPositionB)
    root.exe -b -q 'Db.C("'${d}${table}'",20151101,0)' 
end
