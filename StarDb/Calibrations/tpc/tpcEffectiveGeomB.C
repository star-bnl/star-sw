TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcEffectiveGeom")) return 0;
  tpcEffectiveGeom_st row;
  St_tpcEffectiveGeom *tableSet = new St_tpcEffectiveGeom("tpcEffectiveGeomB",1);
  memset(&row,0,tableSet->GetRowSize());
  row.z_inner_offset	 =      1.157; // cm Garfiled @ V = 1170 V, Roy Bossingham
  row.z_outer_offset	 =      1.627; // cm Garfiled @ V = 1390 V, Roy Bossingham
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
