TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.22871,   0.22892,   0.21583,   0.21470,   0.20723,
       0.21505,   0.20767,   0.18676,   0.18063,   0.18608,
       0.18605,   0.17736,   0.16169,  -0.01561,  -0.01407,
      -0.01589,  -0.01605,  -0.01720,  -0.01503,  -0.02040,
       0.00589,  -0.01072,  -0.01194,  -0.01421,  -0.01601,
      -0.01539,  -0.01515,  -0.01574,  -0.02005,  -0.01850,
      -0.01981,  -0.02898,  -0.02361,  -0.02593,  -0.02520,
      -0.03209,  -0.04077,  -0.04109,  -0.03333,  -0.03229,
      -0.03916,  -0.04410,  -0.04735,  -0.04985,  -0.06644
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
