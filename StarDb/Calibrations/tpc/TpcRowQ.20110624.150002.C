TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.14943,  -0.14941,  -0.15655,  -0.15918,  -0.17029,
      -0.16743,  -0.17449,  -0.20395,  -0.20850,  -0.21812,
      -0.22952,  -0.24194,  -0.25911,  -0.04278,  -0.04098,
      -0.04432,  -0.04511,  -0.04117,  -0.04132,  -0.04632,
      -0.04192,  -0.04082,  -0.04180,  -0.04602,  -0.04927,
      -0.04750,  -0.04544,  -0.04926,  -0.05109,  -0.05196,
      -0.05252,  -0.06267,  -0.05741,  -0.05796,  -0.05748,
      -0.06356,  -0.06744,  -0.05972,  -0.05243,  -0.06096,
      -0.07252,  -0.07442,  -0.07471,  -0.08106,  -0.09517
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
