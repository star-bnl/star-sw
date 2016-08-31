TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.03348,  -0.01753,  -0.00657,   0.00430,   0.01481,
       0.02269,   0.01472,   0.02739,   0.03682,   0.03998,
       0.03728,   0.03113,   0.03047,   0.10287,   0.10779,
       0.10370,   0.10300,   0.10374,   0.10423,   0.09980,
       0.09902,   0.10066,   0.09980,   0.09344,   0.08969,
       0.08767,   0.08841,   0.08003,   0.07749,   0.07836,
       0.07710,   0.06317,   0.06793,   0.06388,   0.06265,
       0.05144,   0.03792,   0.03290,   0.02364,   0.03304,
       0.03127,   0.02569,   0.01938,   0.00614,  -0.01462
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
