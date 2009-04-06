TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_TpcAltroParameters")) return 0;
  St_TpcAltroParameters *tableSet = new St_TpcAltroParameters("TpcAltroParameters",24);
  TpcAltroParameters_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.N = 0;
  for (Int_t i = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
