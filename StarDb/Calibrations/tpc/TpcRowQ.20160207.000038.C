TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.03138,  -0.03349,  -0.03157,  -0.02865,  -0.02365,
      -0.02164,  -0.03343,  -0.02506,  -0.02407,  -0.02801,
      -0.02808,  -0.03203,  -0.03983,   0.09680,   0.10202,
       0.09895,   0.09881,   0.10093,   0.10190,   0.09783,
       0.09715,   0.10000,   0.09918,   0.09346,   0.08961,
       0.08832,   0.08901,   0.08180,   0.07912,   0.08007,
       0.07849,   0.06553,   0.06946,   0.06556,   0.06412,
       0.05365,   0.03967,   0.03412,   0.02440,   0.03430,
       0.03265,   0.02704,   0.02061,   0.00707,  -0.01373
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
