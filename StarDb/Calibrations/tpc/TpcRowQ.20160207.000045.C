TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.15949,  -0.14076,  -0.12937,  -0.11820,  -0.10764,
      -0.10014,  -0.10658,  -0.09251,  -0.08177,  -0.07826,
      -0.08140,  -0.08616,  -0.08815,   0.07648,   0.08134,
       0.07740,   0.07675,   0.07746,   0.07799,   0.07369,
       0.07357,   0.07583,   0.07509,   0.06890,   0.06547,
       0.06351,   0.06435,   0.05424,   0.05122,   0.05248,
       0.05360,   0.03786,   0.04498,   0.04112,   0.04016,
       0.02935,   0.01139,   0.00461,  -0.00519,   0.00509,
       0.00455,  -0.00229,  -0.00828,  -0.02161,  -0.04198
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
