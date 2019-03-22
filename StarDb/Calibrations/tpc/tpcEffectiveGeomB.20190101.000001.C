TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcEffectiveGeom")) return 0;
  tpcEffectiveGeom_st row;
  St_tpcEffectiveGeom *tableSet = new St_tpcEffectiveGeom("tpcEffectiveGeomB",1);
  memset(&row,0,tableSet->GetRowSize());
  row.z_inner_offset	 =      1.3366; // cm Garfiled @ V = 1100 V with Correction from Membrane
  row.z_outer_offset	 =      2.1456; // cm Garfiled @ V = 1390 V
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
