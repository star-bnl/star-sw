TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.18810,  -0.18686,  -0.19070,  -0.19164,  -0.19856,
      -0.19472,  -0.20402,  -0.22779,  -0.22953,  -0.23879,
      -0.24810,  -0.26071,  -0.26506,  -0.04391,  -0.04142,
      -0.04518,  -0.04562,  -0.04112,  -0.04107,  -0.04684,
      -0.04242,  -0.03989,  -0.04045,  -0.04504,  -0.04823,
      -0.04615,  -0.04434,  -0.04938,  -0.05158,  -0.05200,
      -0.05204,  -0.06239,  -0.05765,  -0.05744,  -0.05711,
      -0.06384,  -0.06800,  -0.05901,  -0.05704,  -0.06712,
      -0.07224,  -0.07448,  -0.07525,  -0.08301,  -0.09457
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
