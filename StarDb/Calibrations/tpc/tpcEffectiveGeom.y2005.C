TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcEffectiveGeom")) return 0;
  tpcEffectiveGeom_st row;
  St_tpcEffectiveGeom *tableSet = new St_tpcEffectiveGeom("tpcEffectiveGeom",1);
  memset(&row,0,tableSet->GetRowSize());
  row.z_inner_offset	 =      1.120; // cm: Effective distance between  ;
  row.z_outer_offset	 =      1.627; // cm: Effective distance between  ;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
