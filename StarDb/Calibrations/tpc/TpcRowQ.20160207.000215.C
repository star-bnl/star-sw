TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.18303,  -0.16881,  -0.16104,  -0.15325,  -0.14966,
      -0.14260,  -0.15143,  -0.14141,  -0.13613,  -0.13677,
      -0.14016,  -0.14702,  -0.14993,   0.06561,   0.07050,
       0.06775,   0.06769,   0.06996,   0.07119,   0.06749,
       0.06503,   0.07103,   0.07027,   0.06513,   0.06176,
       0.06130,   0.06249,   0.05541,   0.05118,   0.05538,
       0.05405,   0.03858,   0.04602,   0.04063,   0.03985,
       0.03034,   0.01333,   0.00801,  -0.00192,   0.00937,
       0.00988,   0.00388,  -0.00400,  -0.01681,  -0.03883
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
