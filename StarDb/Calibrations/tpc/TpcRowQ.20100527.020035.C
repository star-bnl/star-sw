TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.22877,   0.22825,   0.21821,   0.22022,   0.20951,
       0.21830,   0.21146,   0.18937,   0.18645,   0.18316,
       0.17967,   0.17381,   0.15490,  -0.02813,  -0.02487,
      -0.02500,  -0.02481,  -0.02299,  -0.02249,  -0.02756,
      -0.02471,  -0.02285,  -0.02479,  -0.02998,  -0.03130,
      -0.02967,  -0.02874,  -0.03254,  -0.02763,  -0.03198,
      -0.03274,  -0.04406,  -0.03920,  -0.03887,  -0.03974,
      -0.04513,  -0.05533,  -0.04861,  -0.04344,  -0.04418,
      -0.05741,  -0.05927,  -0.06202,  -0.06813,  -0.08397
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
