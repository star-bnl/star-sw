TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.19995,  -0.19250,  -0.19621,  -0.18823,  -0.19027,
      -0.18657,  -0.21767,  -0.19573,  -0.19561,  -0.19774,
      -0.20626,  -0.21047,  -0.24050,  -0.01969,  -0.02010,
      -0.02042,  -0.02255,  -0.01669,  -0.02010,  -0.02261,
      -0.02440,  -0.02157,  -0.02328,  -0.02505,  -0.03207,
      -0.03069,  -0.03137,  -0.03369,  -0.03349,  -0.03594,
      -0.03880,  -0.04806,  -0.05014,  -0.04859,  -0.05284,
      -0.05854,  -0.06915,  -0.03048,  -0.06875,  -0.05703,
      -0.07686,  -0.07716,  -0.08171,  -0.09171,  -0.11393
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
