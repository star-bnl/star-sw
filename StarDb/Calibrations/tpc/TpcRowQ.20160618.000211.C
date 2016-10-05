TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.18558,  -0.17377,  -0.16407,  -0.15682,  -0.15538,
      -0.14951,  -0.15709,  -0.14871,  -0.14387,  -0.14102,
      -0.14443,  -0.15172,  -0.16001,   0.05912,   0.06480,
       0.06325,   0.06257,   0.06546,   0.06747,   0.06349,
       0.06165,   0.06778,   0.06772,   0.06293,   0.05956,
       0.05913,   0.06133,   0.05446,   0.05022,   0.05564,
       0.05455,   0.03986,   0.04719,   0.04196,   0.04027,
       0.03152,   0.01382,   0.00923,  -0.00166,   0.01041,
       0.01021,   0.00324,  -0.00539,  -0.01890,  -0.04540
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
