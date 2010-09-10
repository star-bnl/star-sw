TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.09943,   0.10420,   0.09873,   0.09938,   0.09749,
       0.10825,   0.11311,   0.07513,   0.07140,   0.06924,
       0.08549,   0.05757,   0.05932,  -0.05984,  -0.05403,
      -0.05690,  -0.05767,  -0.05464,  -0.05266,  -0.05869,
      -0.05069,  -0.05008,  -0.05150,  -0.05527,  -0.05774,
      -0.05623,  -0.05460,  -0.06075,  -0.05914,  -0.05879,
      -0.05776,  -0.06991,  -0.06494,  -0.06493,  -0.06426,
      -0.07055,  -0.07404,  -0.06890,  -0.06669,  -0.06816,
      -0.07762,  -0.07994,  -0.08225,  -0.08947,  -0.10185
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
