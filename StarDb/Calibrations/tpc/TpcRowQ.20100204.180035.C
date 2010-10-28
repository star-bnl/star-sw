TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.11499,   0.11891,   0.11001,   0.10936,   0.10606,
       0.11793,   0.12292,   0.08374,   0.07751,   0.07447,
       0.09166,   0.06525,   0.06974,  -0.05693,  -0.05275,
      -0.05513,  -0.05575,  -0.05239,  -0.05119,  -0.05628,
      -0.04765,  -0.04815,  -0.04907,  -0.05346,  -0.05565,
      -0.05455,  -0.05207,  -0.05809,  -0.05654,  -0.05691,
      -0.05562,  -0.06815,  -0.06266,  -0.06223,  -0.06219,
      -0.06812,  -0.07119,  -0.06707,  -0.06502,  -0.06636,
      -0.07616,  -0.07824,  -0.08073,  -0.08740,  -0.10074
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
