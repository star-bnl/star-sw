TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.19071,  -0.17758,  -0.16675,  -0.15881,  -0.15651,
      -0.14932,  -0.15621,  -0.14666,  -0.14096,  -0.13699,
      -0.13947,  -0.14555,  -0.15247,   0.06079,   0.06599,
       0.06470,   0.06395,   0.06714,   0.06874,   0.06462,
       0.06235,   0.06811,   0.06795,   0.06291,   0.05933,
       0.05909,   0.06108,   0.05447,   0.05050,   0.05545,
       0.05423,   0.03956,   0.04666,   0.04145,   0.03974,
       0.03085,   0.01363,   0.00945,  -0.00228,   0.01086,
       0.01014,   0.00323,  -0.00559,  -0.01924,  -0.04645
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
