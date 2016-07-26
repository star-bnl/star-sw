TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.26373,  -0.24858,  -0.23926,  -0.22901,  -0.21872,
      -0.20987,  -0.21936,  -0.20237,  -0.19146,  -0.18808,
      -0.19096,  -0.19573,  -0.19707,  -0.14133,  -0.13696,
      -0.14069,  -0.14141,  -0.14101,  -0.14052,  -0.14411,
      -0.14192,  -0.14171,  -0.14248,  -0.14820,  -0.15191,
      -0.15390,  -0.15352,  -0.16173,  -0.16319,  -0.16098,
      -0.16276,  -0.17920,  -0.17176,  -0.17878,  -0.17979,
      -0.18992,  -0.20281,  -0.20675,  -0.21210,  -0.20758,
      -0.20797,  -0.21189,  -0.21916,  -0.23556,  -0.25606
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
