TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.08577,   0.09248,   0.08381,   0.08757,   0.08269,
       0.09879,   0.10456,   0.06641,   0.06512,   0.06407,
       0.06313,   0.05899,   0.07271,  -0.06338,  -0.05819,
      -0.06029,  -0.06074,  -0.05705,  -0.05659,  -0.06034,
      -0.03713,  -0.04878,  -0.04913,  -0.05404,  -0.05613,
      -0.05494,  -0.05267,  -0.05889,  -0.05760,  -0.05561,
      -0.05548,  -0.06803,  -0.06243,  -0.06339,  -0.06171,
      -0.06773,  -0.07113,  -0.06938,  -0.06317,  -0.06572,
      -0.07646,  -0.07820,  -0.08055,  -0.08712,  -0.10052
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
