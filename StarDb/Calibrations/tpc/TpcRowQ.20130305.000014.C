TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.12169,  -0.14423,  -0.15568,  -0.16598,  -0.16978,
      -0.18000,  -0.18118,  -0.20564,  -0.21444,  -0.23050,
      -0.25194,  -0.26471,  -0.25992,  -0.03876,  -0.03984,
      -0.04505,  -0.04654,  -0.04462,  -0.04668,  -0.05092,
      -0.04886,  -0.05135,  -0.05364,  -0.05773,  -0.06290,
      -0.06291,  -0.06378,  -0.06834,  -0.07043,  -0.07169,
      -0.07249,  -0.08388,  -0.07912,  -0.07980,  -0.08125,
      -0.08717,  -0.09354,  -0.08770,  -0.09290,  -0.09640,
      -0.09750,  -0.10219,  -0.10337,  -0.11324,  -0.12638
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
