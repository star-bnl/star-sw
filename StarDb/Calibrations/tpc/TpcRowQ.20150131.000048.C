TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.19230,  -0.18121,  -0.18834,  -0.18481,  -0.18718,
      -0.18636,  -0.21876,  -0.20357,  -0.20308,  -0.20553,
      -0.21581,  -0.22817,  -0.24886,  -0.06953,  -0.06624,
      -0.07336,  -0.07323,  -0.06769,  -0.06751,  -0.07491,
      -0.07327,  -0.07271,  -0.07741,  -0.08271,  -0.08701,
      -0.08759,  -0.08744,  -0.09262,  -0.09220,  -0.09556,
      -0.09764,  -0.10964,  -0.10575,  -0.10901,  -0.11006,
      -0.11927,  -0.12699,  -0.14346,  -0.14684,  -0.13405,
      -0.13495,  -0.14007,  -0.14527,  -0.15594,  -0.17809
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
