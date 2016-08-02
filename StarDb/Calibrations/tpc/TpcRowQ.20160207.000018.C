TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.07461,  -0.05966,  -0.05208,  -0.04212,  -0.03455,
      -0.02619,  -0.02247,  -0.02111,  -0.01331,  -0.01148,
      -0.01288,  -0.01615,  -0.01639,   0.13325,   0.13800,
       0.13503,   0.13506,   0.13825,   0.13931,   0.13491,
       0.13611,   0.13864,   0.13801,   0.13304,   0.12959,
       0.12955,   0.13124,   0.12543,   0.12349,   0.12595,
       0.12465,   0.11146,   0.11596,   0.11247,   0.11119,
       0.10142,   0.09289,   0.09605,   0.09637,   0.08696,
       0.08290,   0.07673,   0.07057,   0.05740,   0.03657
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
