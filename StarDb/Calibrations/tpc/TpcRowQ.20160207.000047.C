TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.16555,  -0.15173,  -0.14211,  -0.13531,  -0.12491,
      -0.11864,  -0.12778,  -0.11670,  -0.10677,  -0.10454,
      -0.10640,  -0.11116,  -0.11315,   0.06055,   0.06542,
       0.06172,   0.06095,   0.06188,   0.06223,   0.05801,
       0.05799,   0.06061,   0.05938,   0.05295,   0.04910,
       0.04691,   0.04734,   0.03918,   0.03531,   0.03686,
       0.03523,   0.02176,   0.02588,   0.02171,   0.02042,
       0.00929,  -0.00649,  -0.01371,  -0.02341,  -0.01382,
      -0.01468,  -0.02186,  -0.02807,  -0.04156,  -0.06135
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
