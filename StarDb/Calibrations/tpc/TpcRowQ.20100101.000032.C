TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.18865,   0.18749,   0.17550,   0.17534,   0.16509,
       0.17478,   0.16908,   0.14187,   0.13451,   0.13722,
       0.13303,   0.12489,   0.10688,  -0.04270,  -0.03936,
      -0.04010,  -0.04036,  -0.04000,  -0.03931,  -0.04427,
      -0.03275,  -0.03713,  -0.03846,  -0.04274,  -0.04546,
      -0.04482,  -0.04264,  -0.04680,  -0.04625,  -0.04746,
      -0.04815,  -0.05876,  -0.05276,  -0.05420,  -0.05412,
      -0.05956,  -0.06465,  -0.06417,  -0.05827,  -0.05945,
      -0.06938,  -0.07261,  -0.07462,  -0.07972,  -0.09552
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
