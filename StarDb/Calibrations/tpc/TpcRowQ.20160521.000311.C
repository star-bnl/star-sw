TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.04767,   0.05223,   0.05103,   0.05385,   0.04956,
       0.05399,   0.04080,   0.04796,   0.05151,   0.04960,
       0.04661,   0.03886,   0.01673,   0.10664,   0.11215,
       0.10979,   0.11076,   0.11410,   0.11655,   0.11136,
       0.11258,   0.11463,   0.11377,   0.10903,   0.10594,
       0.10575,   0.10841,   0.10291,   0.09993,   0.10217,
       0.10084,   0.08542,   0.09297,   0.08774,   0.08734,
       0.07803,   0.06450,   0.05869,   0.04862,   0.06344,
       0.05995,   0.05361,   0.04770,   0.03621,   0.00678
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
