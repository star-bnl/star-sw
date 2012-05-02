TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcAltroParams")) return 0;
  St_tpcAltroParams *tableSet = new St_tpcAltroParams("tpcAltroParams",24);
  tpcAltroParams_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.N = -1; // TPC only
  for (Int_t i  = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
