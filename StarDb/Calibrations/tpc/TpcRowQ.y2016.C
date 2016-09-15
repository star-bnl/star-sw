TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.16868,  -0.15012,  -0.13883,  -0.13044,  -0.12500,
      -0.11934,  -0.12500,  -0.11550,  -0.10885,  -0.10818,
      -0.10939,  -0.11411,  -0.11595,   0.06084,   0.06537,
       0.06204,   0.06153,   0.06236,   0.06265,   0.05874,
       0.05889,   0.06112,   0.05984,   0.05375,   0.04996,
       0.04775,   0.04812,   0.03977,   0.03578,   0.03744,
       0.03581,   0.01960,   0.02595,   0.01960,   0.01846,
       0.00867,  -0.00619,  -0.01351,  -0.02318,  -0.01361,
      -0.01440,  -0.02174,  -0.02984,  -0.04238,  -0.06106
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
