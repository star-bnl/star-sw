TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcPadGainT0B")) return 0;
  St_tpcPadGainT0B *tableSet = new St_tpcPadGainT0B("tpcPadGainT0B",24);
  tpcPadGainT0B_st row;
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t r = 1; r <= 100; r++) {
    for (Int_t p = 1; p <= 182; p++) {
      row.Gain[r-1][p-1] = 1.;
      row.T0[r-1][p-1] = 0;
    }
  }
  for (Int_t s = 1; s <= 24; s++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
