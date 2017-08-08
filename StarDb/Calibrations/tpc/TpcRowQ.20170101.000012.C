TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.24144,  -0.21485,  -0.20079,  -0.18410,  -0.17446,
      -0.16506,  -0.15772,  -0.16013,  -0.15230,  -0.14997,
      -0.15219,  -0.15697,  -0.14949,   0.08976,   0.09288,
       0.09064,   0.09107,   0.09438,   0.09610,   0.09204,
       0.09650,   0.09687,   0.09652,   0.09156,   0.08811,
       0.08849,   0.09035,   0.08479,   0.08348,   0.08405,
       0.08217,   0.06948,   0.07467,   0.07086,   0.06928,
       0.05997,   0.05120,   0.05318,   0.05406,   0.04513,
       0.04037,   0.03264,   0.02362,   0.00528,  -0.03010
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
