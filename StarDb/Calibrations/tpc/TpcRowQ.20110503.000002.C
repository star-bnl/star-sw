TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.17772,  -0.17713,  -0.18214,  -0.18388,  -0.19404,
      -0.18820,  -0.20061,  -0.22616,  -0.22688,  -0.23568,
      -0.24700,  -0.25869,  -0.26365,  -0.02823,  -0.02626,
      -0.02961,  -0.02995,  -0.02590,  -0.02565,  -0.03134,
      -0.02628,  -0.02391,  -0.02420,  -0.02867,  -0.03133,
      -0.02985,  -0.02794,  -0.03267,  -0.03470,  -0.03581,
      -0.03596,  -0.04566,  -0.04066,  -0.04132,  -0.04082,
      -0.04668,  -0.05004,  -0.04253,  -0.04261,  -0.05246,
      -0.05445,  -0.05755,  -0.05731,  -0.06385,  -0.07585
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
