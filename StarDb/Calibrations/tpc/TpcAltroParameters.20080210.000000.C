TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_TpcAltroParameters")) return 0;
  TpcAltroParameters_st row;
  St_TpcAltroParameters *tableSet = new St_TpcAltroParameters("TpcAltroParameters",24);
  for (Int_t i = 0; i < 24; i++) {
    memset(&row,0,tableSet->GetRowSize());
    row.N = -1; // TPC only
    if (i == 15) { // TPX sector 16 in run VIII, Tonko ALTRO constants for pp, April 11, 2008
      row.N = 0;
    }
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
