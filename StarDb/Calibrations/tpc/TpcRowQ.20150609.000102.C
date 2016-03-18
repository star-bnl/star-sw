TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.21713,  -0.20103,  -0.21302,  -0.20087,  -0.20212,
      -0.20045,  -0.23104,  -0.21738,  -0.21644,  -0.20913,
      -0.22145,  -0.22965,  -0.24597,  -0.07465,  -0.07118,
      -0.07665,  -0.07810,  -0.07159,  -0.07108,  -0.07934,
      -0.07711,  -0.07602,  -0.07929,  -0.08323,  -0.08721,
      -0.08743,  -0.08732,  -0.09190,  -0.09426,  -0.09536,
      -0.09602,  -0.10780,  -0.10446,  -0.10805,  -0.10996,
      -0.11845,  -0.12839,  -0.14191,  -0.14468,  -0.13412,
      -0.13355,  -0.13784,  -0.14471,  -0.15611,  -0.18022
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
