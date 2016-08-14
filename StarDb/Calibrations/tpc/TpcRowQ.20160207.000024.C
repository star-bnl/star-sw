TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.00077,   0.01490,   0.02221,   0.03131,   0.04032,
       0.04766,   0.04120,   0.05081,   0.05689,   0.05883,
       0.05582,   0.05075,   0.04797,   0.10459,   0.10912,
       0.10497,   0.10509,   0.10619,   0.10733,   0.10403,
       0.10679,   0.10743,   0.10735,   0.10198,   0.09878,
       0.09728,   0.09879,   0.09084,   0.09028,   0.09285,
       0.09179,   0.07826,   0.08341,   0.07973,   0.07885,
       0.06740,   0.05353,   0.04892,   0.04192,   0.04931,
       0.04995,   0.04628,   0.03935,   0.02428,   0.00247
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
