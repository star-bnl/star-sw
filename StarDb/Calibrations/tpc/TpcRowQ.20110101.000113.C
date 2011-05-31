TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.15214,   0.14569,   0.13344,   0.12902,   0.11782,
       0.12449,   0.11290,   0.08345,   0.08189,   0.07319,
       0.06612,   0.05815,   0.05402,  -0.05262,  -0.04986,
      -0.05238,  -0.05272,  -0.04803,  -0.04659,  -0.05202,
      -0.04978,  -0.04820,  -0.04897,  -0.05307,  -0.05581,
      -0.05360,  -0.05127,  -0.05588,  -0.05429,  -0.05509,
      -0.05415,  -0.06556,  -0.05951,  -0.05999,  -0.05960,
      -0.06551,  -0.06916,  -0.06348,  -0.05699,  -0.07060,
      -0.07266,  -0.07570,  -0.07507,  -0.08110,  -0.09507
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
