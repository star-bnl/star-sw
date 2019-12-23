TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcEffectiveGeom")) return 0;
  tpcEffectiveGeom_st row;
  St_tpcEffectiveGeom *tableSet = new St_tpcEffectiveGeom("tpcEffectiveGeomB",1);
  memset(&row,0,tableSet->GetRowSize());
  row.z_inner_offset	 =      1.1942 -2.64982e-01; // cm include overall T0, from membrane position
  row.z_outer_offset	 =      1.7081 -6.09363e-01; // cm 
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
