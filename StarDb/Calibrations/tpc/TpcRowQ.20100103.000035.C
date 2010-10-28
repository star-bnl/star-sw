TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
       0.10556,   0.10875,   0.09775,   0.09972,   0.09190,
       0.11043,   0.11488,   0.07698,   0.07526,   0.07391,
       0.07341,   0.07037,   0.08341,  -0.06129,  -0.05618,
      -0.05766,  -0.05800,  -0.05379,  -0.05293,  -0.05653,
      -0.03370,  -0.04631,  -0.04627,  -0.05126,  -0.05385,
      -0.05202,  -0.04991,  -0.05576,  -0.05410,  -0.05323,
      -0.05324,  -0.06583,  -0.05955,  -0.05998,  -0.05899,
      -0.06518,  -0.06782,  -0.06778,  -0.06089,  -0.06326,
      -0.07394,  -0.07639,  -0.07802,  -0.08422,  -0.09816
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
