TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.18231,   0.18269,   0.17244,   0.17152,   0.16260,
       0.17089,   0.16403,   0.13775,   0.13039,   0.12299,
       0.11961,   0.11157,   0.09407,  -0.05671,  -0.05211,
      -0.05591,  -0.05535,  -0.05286,  -0.05194,  -0.05740,
      -0.05003,  -0.04990,  -0.05040,  -0.05695,  -0.05872,
      -0.05741,  -0.05519,  -0.06001,  -0.05688,  -0.05839,
      -0.05924,  -0.07148,  -0.06412,  -0.06547,  -0.06518,
      -0.07127,  -0.07344,  -0.07412,  -0.06831,  -0.07068,
      -0.08088,  -0.08271,  -0.08446,  -0.09105,  -0.10691
  };
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",45);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.nrows = 45;
  row.npar  =  1;
  for (Int_t i = 0; i < 45; i++) {
    row.idx  = i + 1;
    row.a[0] = rowsGain[i];
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
