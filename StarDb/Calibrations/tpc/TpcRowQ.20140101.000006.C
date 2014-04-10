TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.14275,  -0.14068,  -0.14195,  -0.13959,  -0.14460,
      -0.14528,  -0.14641,  -0.16649,  -0.16846,  -0.17737,
      -0.18708,  -0.19758,  -0.22238,  -0.03672,  -0.03590,
      -0.03790,  -0.03987,  -0.03746,  -0.03776,  -0.04314,
      -0.04009,  -0.04043,  -0.04228,  -0.04658,  -0.05084,
      -0.05070,  -0.04951,  -0.05413,  -0.05273,  -0.05550,
      -0.05759,  -0.06732,  -0.06355,  -0.06555,  -0.06592,
      -0.07233,  -0.07778,  -0.07005,  -0.07329,  -0.07750,
      -0.08282,  -0.08592,  -0.08798,  -0.09481,  -0.11204
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
