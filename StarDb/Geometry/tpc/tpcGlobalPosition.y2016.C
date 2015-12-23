TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcGlobalPosition")) return 0;
  tpcGlobalPosition_st row;
  St_tpcGlobalPosition *tableSet = new St_tpcGlobalPosition("tpcGlobalPosition",1);
  memset(&row,0,tableSet->GetRowSize());
  row.XX	 =          1; // XX element of rotation matrix  ;
  row.YY	 =          1; // YY element of rotation matrix  ;
  row.ZZ	 =          1; // ZZ element of rotation matrix  ;
  row.XX_geom	 =          1; // XX element of geometrical rotation matrix  ;
  row.YY_geom	 =          1; // YY element of geometrical rotation matrix  ;
  row.ZZ_geom	 =          1; // ZZ element of geometrical rotation matrix  ;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
