TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.17421,  -0.17255,  -0.17782,  -0.17753,  -0.18663,
      -0.18039,  -0.19272,  -0.21682,  -0.21562,  -0.22253,
      -0.23217,  -0.24219,  -0.24591,  -0.02483,  -0.02243,
      -0.02562,  -0.02565,  -0.02100,  -0.02054,  -0.02579,
      -0.02107,  -0.02047,  -0.02049,  -0.02447,  -0.02717,
      -0.02577,  -0.02375,  -0.02857,  -0.03028,  -0.03153,
      -0.03164,  -0.04148,  -0.03622,  -0.03689,  -0.03629,
      -0.04219,  -0.04583,  -0.03877,  -0.03897,  -0.04899,
      -0.05080,  -0.05360,  -0.05316,  -0.06014,  -0.07210
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
