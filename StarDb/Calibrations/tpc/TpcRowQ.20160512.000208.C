TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.14730,  -0.13071,  -0.12601,  -0.11892,  -0.11718,
      -0.11101,  -0.12078,  -0.11258,  -0.10767,  -0.10759,
      -0.11098,  -0.11806,  -0.12582,   0.07778,   0.08284,
       0.08030,   0.08119,   0.08437,   0.08655,   0.08191,
       0.08315,   0.08782,   0.08779,   0.08237,   0.07955,
       0.08006,   0.08212,   0.07593,   0.07206,   0.07650,
       0.07551,   0.05961,   0.06757,   0.06231,   0.06210,
       0.05256,   0.03655,   0.03306,   0.02281,   0.03587,
       0.03376,   0.02754,   0.02002,   0.00779,  -0.01887
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
