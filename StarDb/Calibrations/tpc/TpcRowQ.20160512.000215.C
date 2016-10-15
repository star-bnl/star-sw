TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.15514,  -0.13728,  -0.13106,  -0.12272,  -0.12035,
      -0.11292,  -0.12188,  -0.11240,  -0.10634,  -0.10506,
      -0.10724,  -0.11346,  -0.12011,   0.07663,   0.08177,
       0.07915,   0.07979,   0.08302,   0.08481,   0.08012,
       0.08114,   0.08509,   0.08503,   0.07959,   0.07658,
       0.07710,   0.07897,   0.07290,   0.06955,   0.07355,
       0.07216,   0.05655,   0.06434,   0.05918,   0.05879,
       0.04942,   0.03360,   0.03017,   0.01948,   0.03330,
       0.03107,   0.02496,   0.01717,   0.00509,  -0.02192
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
