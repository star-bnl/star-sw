TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.12988,  -0.12945,  -0.13497,  -0.13698,  -0.14775,
      -0.14479,  -0.15067,  -0.17883,  -0.18304,  -0.19218,
      -0.20153,  -0.21366,  -0.23084,  -0.01810,  -0.01619,
      -0.01941,  -0.02004,  -0.01591,  -0.01634,  -0.02112,
      -0.01717,  -0.01434,  -0.01524,  -0.01920,  -0.02248,
      -0.02089,  -0.01901,  -0.02308,  -0.02575,  -0.02702,
      -0.02739,  -0.03687,  -0.03266,  -0.03318,  -0.03271,
      -0.03860,  -0.04237,  -0.03579,  -0.03113,  -0.04591,
      -0.04828,  -0.05016,  -0.05055,  -0.05660,  -0.07078
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
