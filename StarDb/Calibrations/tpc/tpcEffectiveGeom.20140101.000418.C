TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcEffectiveGeom")) return 0;
  tpcEffectiveGeom_st row;
  St_tpcEffectiveGeom *tableSet = new St_tpcEffectiveGeom("tpcEffectiveGeom",1);
  memset(&row,0,tableSet->GetRowSize());
  row.z_inner_offset	 =      1.1942; // cm Garfiled @ V = 1100 V 
  row.z_outer_offset	 =      1.7081; // cm Garfiled @ V = 1390 V
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
