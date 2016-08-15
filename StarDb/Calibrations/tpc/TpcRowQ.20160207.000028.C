TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.00161,   0.01618,   0.02366,   0.03301,   0.04209,
       0.04954,   0.04077,   0.05229,   0.05841,   0.05840,
       0.05552,   0.05054,   0.04799,   0.10626,   0.11083,
       0.10675,   0.10689,   0.10813,   0.10936,   0.10625,
       0.10870,   0.10960,   0.10957,   0.10424,   0.10102,
       0.09949,   0.10093,   0.09271,   0.09223,   0.09499,
       0.09391,   0.08037,   0.08552,   0.08192,   0.08095,
       0.06932,   0.05413,   0.04946,   0.04199,   0.05097,
       0.05201,   0.04847,   0.04151,   0.02613,   0.00306
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
