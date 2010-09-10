TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.18266,   0.18205,   0.17143,   0.17130,   0.16042,
       0.17013,   0.16377,   0.13691,   0.13138,   0.12296,
       0.11830,   0.11138,   0.09381,  -0.05678,  -0.05232,
      -0.05544,  -0.05474,  -0.05195,  -0.05097,  -0.05729,
      -0.04903,  -0.04912,  -0.05056,  -0.05550,  -0.05798,
      -0.05674,  -0.05434,  -0.05931,  -0.05619,  -0.05758,
      -0.05846,  -0.07061,  -0.06348,  -0.06485,  -0.06467,
      -0.07057,  -0.07311,  -0.07321,  -0.06698,  -0.07060,
      -0.08008,  -0.08189,  -0.08370,  -0.09115,  -0.10619
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
