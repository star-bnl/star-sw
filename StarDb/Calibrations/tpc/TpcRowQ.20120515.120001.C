TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.16250,  -0.15829,  -0.16311,  -0.16338,  -0.17061,
      -0.17090,  -0.18192,  -0.20799,  -0.22277,  -0.23771,
      -0.25350,  -0.27282,  -0.27806,  -0.05344,  -0.05108,
      -0.05770,  -0.05746,  -0.05396,  -0.05447,  -0.05853,
      -0.05520,  -0.05628,  -0.05697,  -0.06268,  -0.06777,
      -0.06566,  -0.06490,  -0.06961,  -0.07096,  -0.07355,
      -0.07618,  -0.08657,  -0.08228,  -0.08473,  -0.08517,
      -0.08987,  -0.09475,  -0.08726,  -0.08922,  -0.09725,
      -0.10105,  -0.10334,  -0.10610,  -0.11245,  -0.12715
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
