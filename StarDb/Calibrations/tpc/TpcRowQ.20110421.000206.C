TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.13955,  -0.14232,  -0.15017,  -0.15262,  -0.16414,
      -0.15905,  -0.17002,  -0.19597,  -0.19766,  -0.20452,
      -0.21155,  -0.21915,  -0.23257,  -0.02951,  -0.02708,
      -0.02900,  -0.02949,  -0.02558,  -0.02522,  -0.02992,
      -0.02537,  -0.02361,  -0.02383,  -0.02802,  -0.03067,
      -0.02903,  -0.02689,  -0.03068,  -0.03137,  -0.03220,
      -0.03270,  -0.04178,  -0.03689,  -0.03812,  -0.03748,
      -0.04322,  -0.04715,  -0.04049,  -0.03944,  -0.04771,
      -0.05256,  -0.05479,  -0.05466,  -0.06053,  -0.07444
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
