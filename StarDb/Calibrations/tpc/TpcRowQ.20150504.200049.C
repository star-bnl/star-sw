TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.19424,  -0.18158,  -0.19115,  -0.18742,  -0.18906,
      -0.18877,  -0.22210,  -0.20543,  -0.20510,  -0.20712,
      -0.21535,  -0.22561,  -0.24886,  -0.07360,  -0.06919,
      -0.07615,  -0.07548,  -0.06982,  -0.06951,  -0.07699,
      -0.07421,  -0.07472,  -0.07990,  -0.08473,  -0.08800,
      -0.08808,  -0.08729,  -0.09397,  -0.09268,  -0.09615,
      -0.09822,  -0.10982,  -0.10581,  -0.10997,  -0.11103,
      -0.11963,  -0.12741,  -0.14358,  -0.14730,  -0.13643,
      -0.13567,  -0.14128,  -0.14715,  -0.15725,  -0.18072
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
