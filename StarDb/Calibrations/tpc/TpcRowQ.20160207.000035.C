TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.00116,   0.01564,   0.02449,   0.03376,   0.04279,
       0.05018,   0.04246,   0.05239,   0.05848,   0.05835,
       0.05561,   0.05069,   0.04811,   0.10692,   0.11178,
       0.10791,   0.10813,   0.10969,   0.11093,   0.10760,
       0.10770,   0.10998,   0.11005,   0.10465,   0.10184,
       0.10067,   0.10247,   0.09470,   0.09323,   0.09480,
       0.09426,   0.08090,   0.08597,   0.08265,   0.08182,
       0.07115,   0.05683,   0.04946,   0.03538,   0.05208,
       0.05241,   0.04572,   0.03986,   0.02614,   0.00585
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
