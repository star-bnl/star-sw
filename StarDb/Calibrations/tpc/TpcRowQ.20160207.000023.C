TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.08323,  -0.06806,  -0.05992,  -0.04917,  -0.04132,
      -0.03251,  -0.02775,  -0.02675,  -0.01842,  -0.01599,
      -0.01728,  -0.02036,  -0.02030,   0.12667,   0.13138,
       0.12874,   0.12913,   0.13265,   0.13381,   0.12954,
       0.13097,   0.13357,   0.13309,   0.12814,   0.12484,
       0.12477,   0.12652,   0.12075,   0.11872,   0.12137,
       0.12015,   0.10695,   0.11152,   0.10811,   0.10686,
       0.09708,   0.08849,   0.09162,   0.09188,   0.08247,
       0.07830,   0.07238,   0.06609,   0.05279,   0.03181
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
