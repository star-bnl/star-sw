TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.21811,   0.21505,   0.20474,   0.20695,   0.19678,
       0.20678,   0.19815,   0.17514,   0.17089,   0.16669,
       0.16251,   0.15789,   0.13992,  -0.03257,  -0.02806,
      -0.02773,  -0.02798,  -0.02719,  -0.02653,  -0.03070,
      -0.02922,  -0.02683,  -0.02811,  -0.03318,  -0.03412,
      -0.03345,  -0.03251,  -0.03534,  -0.03121,  -0.03653,
      -0.03605,  -0.04717,  -0.04149,  -0.04269,  -0.04146,
      -0.04821,  -0.05702,  -0.05011,  -0.04685,  -0.04702,
      -0.05966,  -0.06220,  -0.06481,  -0.06962,  -0.08559
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
