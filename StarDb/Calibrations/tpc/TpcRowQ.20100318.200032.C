TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.12904,   0.13451,   0.12889,   0.12991,   0.12362,
       0.13449,   0.12956,   0.10082,   0.09474,   0.08899,
       0.08425,   0.07780,   0.06287,  -0.05848,  -0.05491,
      -0.05648,  -0.05671,  -0.05410,  -0.05282,  -0.05865,
      -0.05492,  -0.05222,  -0.05329,  -0.05843,  -0.06091,
      -0.05910,  -0.05732,  -0.06302,  -0.06141,  -0.06094,
      -0.06089,  -0.07310,  -0.06732,  -0.06701,  -0.06687,
      -0.07360,  -0.07611,  -0.07522,  -0.07022,  -0.07201,
      -0.08308,  -0.08451,  -0.08651,  -0.09353,  -0.10821
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
