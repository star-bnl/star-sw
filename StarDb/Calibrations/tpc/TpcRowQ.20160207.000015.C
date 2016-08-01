TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.07847,  -0.06418,  -0.05603,  -0.04634,  -0.03859,
      -0.03055,  -0.02667,  -0.02481,  -0.01712,  -0.01493,
      -0.01608,  -0.01933,  -0.01937,   0.13134,   0.13610,
       0.13323,   0.13319,   0.13616,   0.13746,   0.13319,
       0.13447,   0.13709,   0.13673,   0.13176,   0.12839,
       0.12833,   0.12975,   0.12384,   0.12186,   0.12438,
       0.12302,   0.10996,   0.11454,   0.11089,   0.10961,
       0.09997,   0.09092,   0.09271,   0.09338,   0.08483,
       0.08141,   0.07518,   0.06900,   0.05566,   0.03472
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
