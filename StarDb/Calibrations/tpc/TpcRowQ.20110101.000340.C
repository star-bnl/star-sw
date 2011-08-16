TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.15843,  -0.16553,  -0.17938,  -0.18471,  -0.19620,
      -0.19038,  -0.20259,  -0.23296,  -0.23534,  -0.24519,
      -0.25157,  -0.26039,  -0.26434,  -0.04680,  -0.04408,
      -0.04701,  -0.04683,  -0.04234,  -0.04140,  -0.04636,
      -0.04325,  -0.04090,  -0.04154,  -0.04559,  -0.04814,
      -0.04612,  -0.04374,  -0.04800,  -0.04897,  -0.04997,
      -0.04955,  -0.06024,  -0.05436,  -0.05504,  -0.05429,
      -0.05985,  -0.06408,  -0.05750,  -0.05481,  -0.06742,
      -0.06606,  -0.06948,  -0.06873,  -0.07491,  -0.08799
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
