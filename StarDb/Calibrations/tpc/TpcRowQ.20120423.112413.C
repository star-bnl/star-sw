TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.19462,  -0.18896,  -0.19221,  -0.19333,  -0.19827,
      -0.19138,  -0.20190,  -0.22644,  -0.23457,  -0.24944,
      -0.26167,  -0.27809,  -0.28762,  -0.05583,  -0.05343,
      -0.05821,  -0.05929,  -0.05507,  -0.05653,  -0.06072,
      -0.05578,  -0.05411,  -0.05654,  -0.06192,  -0.06574,
      -0.06426,  -0.06330,  -0.06843,  -0.07168,  -0.07281,
      -0.07388,  -0.08386,  -0.07988,  -0.08125,  -0.08148,
      -0.08828,  -0.09328,  -0.08548,  -0.08758,  -0.09513,
      -0.09864,  -0.10157,  -0.10327,  -0.11073,  -0.12502
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
