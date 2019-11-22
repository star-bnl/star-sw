TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_itpcPadGainT0")) return 0;
  itpcPadGainT0_st row;
  St_itpcPadGainT0 *tableSet = new St_itpcPadGainT0("itpcPadGainT0",1);
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t s = 1; s <= 24; s++)
    for (Int_t r = 1; r <= 40; r++)
      for (Int_t p = 1; p <= 120; p++) {
	row.Gain[s-1][r-1][p-1] = 1.;
	row.T0[s-1][r-1][p-1] = 0;
      }
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
