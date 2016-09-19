TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.17827,  -0.16492,  -0.15813,  -0.15128,  -0.14835,
      -0.14248,  -0.15242,  -0.14349,  -0.13932,  -0.14129,
      -0.14565,  -0.15356,  -0.15760,   0.06488,   0.06971,
       0.06715,   0.06718,   0.06934,   0.07069,   0.06721,
       0.06522,   0.07147,   0.07084,   0.06570,   0.06248,
       0.06195,   0.06320,   0.05603,   0.05130,   0.05607,
       0.05479,   0.03929,   0.04678,   0.04139,   0.04067,
       0.03119,   0.01376,   0.00842,  -0.00105,   0.00918,
       0.00996,   0.00410,  -0.00375,  -0.01656,  -0.03811
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
