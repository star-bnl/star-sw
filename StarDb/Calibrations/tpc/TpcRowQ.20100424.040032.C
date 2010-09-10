TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.22479,   0.22329,   0.21212,   0.21149,   0.20171,
       0.20990,   0.20288,   0.17983,   0.17293,   0.17988,
       0.17521,   0.17026,   0.15394,  -0.01719,  -0.01427,
      -0.01563,  -0.01671,  -0.01626,  -0.01587,  -0.02103,
       0.00306,  -0.01117,  -0.01309,  -0.01691,  -0.01947,
      -0.01904,  -0.01736,  -0.02094,  -0.02405,  -0.02159,
      -0.02263,  -0.03133,  -0.02639,  -0.02926,  -0.02875,
      -0.03345,  -0.04463,  -0.04461,  -0.03527,  -0.03474,
      -0.04241,  -0.04745,  -0.04917,  -0.05361,  -0.06854
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
