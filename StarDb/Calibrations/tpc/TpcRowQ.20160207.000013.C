TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.08673,  -0.07242,  -0.06385,  -0.05344,  -0.04544,
      -0.03702,  -0.03199,  -0.03069,  -0.02251,  -0.01992,
      -0.02102,  -0.02420,  -0.02428,   0.12523,   0.12992,
       0.12730,   0.12764,   0.13092,   0.13233,   0.12814,
       0.12950,   0.13228,   0.13197,   0.12705,   0.12377,
       0.12366,   0.12515,   0.11926,   0.11720,   0.11987,
       0.11855,   0.10556,   0.11012,   0.10659,   0.10528,
       0.09567,   0.08649,   0.08828,   0.08898,   0.08041,
       0.07706,   0.07087,   0.06461,   0.05126,   0.03024
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
