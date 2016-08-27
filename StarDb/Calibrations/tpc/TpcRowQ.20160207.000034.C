TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.00131,   0.01584,   0.02487,   0.03401,   0.04304,
       0.05050,   0.04271,   0.05250,   0.05857,   0.05846,
       0.05564,   0.05077,   0.04810,   0.10700,   0.11184,
       0.10788,   0.10816,   0.10968,   0.11092,   0.10755,
       0.10767,   0.10995,   0.11007,   0.10459,   0.10186,
       0.10068,   0.10238,   0.09476,   0.09320,   0.09475,
       0.09416,   0.08091,   0.08599,   0.08267,   0.08187,
       0.07116,   0.05683,   0.04955,   0.03545,   0.05212,
       0.05250,   0.04573,   0.03988,   0.02614,   0.00586
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
