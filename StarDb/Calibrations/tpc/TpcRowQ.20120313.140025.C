TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.18465,  -0.18493,  -0.19731,  -0.20511,  -0.21285,
      -0.21145,  -0.22429,  -0.25773,  -0.27006,  -0.28570,
      -0.30245,  -0.31795,  -0.31278,  -0.07059,  -0.06879,
      -0.07363,  -0.07417,  -0.07004,  -0.07114,  -0.07631,
      -0.07252,  -0.07201,  -0.07368,  -0.07773,  -0.08111,
      -0.07959,  -0.07830,  -0.08348,  -0.08506,  -0.08643,
      -0.08640,  -0.09700,  -0.09253,  -0.09346,  -0.09365,
      -0.09957,  -0.10411,  -0.09769,  -0.09862,  -0.10560,
      -0.10835,  -0.11031,  -0.11262,  -0.12079,  -0.13290
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
