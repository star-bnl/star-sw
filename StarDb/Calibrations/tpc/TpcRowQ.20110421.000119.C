TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.16517,   0.16318,   0.15584,   0.15472,   0.14392,
       0.15120,   0.14026,   0.11630,   0.11365,   0.10866,
       0.10215,   0.09571,   0.08338,  -0.03178,  -0.02960,
      -0.03100,  -0.03180,  -0.02688,  -0.02614,  -0.03156,
      -0.02918,  -0.02829,  -0.02888,  -0.03288,  -0.03607,
      -0.03430,  -0.03220,  -0.03630,  -0.03400,  -0.03435,
      -0.03407,  -0.04381,  -0.03883,  -0.04050,  -0.03994,
      -0.04562,  -0.04966,  -0.04523,  -0.04191,  -0.05591,
      -0.05578,  -0.05721,  -0.05747,  -0.06291,  -0.07805
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
