TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.18884,  -0.18387,  -0.18524,  -0.18199,  -0.18324,
      -0.18358,  -0.19267,  -0.19903,  -0.19716,  -0.20229,
      -0.21298,  -0.22231,  -0.24228,  -0.05331,  -0.04990,
      -0.05307,  -0.05428,  -0.05015,  -0.04948,  -0.05511,
      -0.04914,  -0.05556,  -0.05756,  -0.06325,  -0.06741,
      -0.06681,  -0.06642,  -0.07128,  -0.06975,  -0.07431,
      -0.07637,  -0.08833,  -0.08493,  -0.08845,  -0.08934,
      -0.09751,  -0.10121,  -0.08437,  -0.08723,  -0.10261,
      -0.11093,  -0.11876,  -0.12345,  -0.13344,  -0.15422
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
