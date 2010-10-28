TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.20328,   0.20077,   0.18663,   0.18516,   0.17338,
       0.18432,   0.17815,   0.15081,   0.14220,   0.14459,
       0.14121,   0.13391,   0.11669,  -0.04048,  -0.03790,
      -0.03823,  -0.03837,  -0.03765,  -0.03711,  -0.04157,
      -0.02996,  -0.03524,  -0.03613,  -0.04085,  -0.04362,
      -0.04281,  -0.04038,  -0.04432,  -0.04363,  -0.04561,
      -0.04631,  -0.05709,  -0.05058,  -0.05154,  -0.05210,
      -0.05746,  -0.06202,  -0.06281,  -0.05664,  -0.05761,
      -0.06770,  -0.07114,  -0.07291,  -0.07759,  -0.09411
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
