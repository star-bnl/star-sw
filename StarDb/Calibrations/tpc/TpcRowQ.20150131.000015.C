TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.14354,  -0.13881,  -0.14047,  -0.13690,  -0.13810,
      -0.13880,  -0.14791,  -0.15394,  -0.15184,  -0.16284,
      -0.17318,  -0.18322,  -0.20330,  -0.02939,  -0.02615,
      -0.02884,  -0.03020,  -0.02625,  -0.02534,  -0.03093,
      -0.02456,  -0.02938,  -0.03132,  -0.03704,  -0.04149,
      -0.04078,  -0.04071,  -0.04514,  -0.04363,  -0.04827,
      -0.05024,  -0.06201,  -0.05876,  -0.06224,  -0.06306,
      -0.07147,  -0.07512,  -0.05984,  -0.06296,  -0.07803,
      -0.08638,  -0.09433,  -0.09878,  -0.10875,  -0.12947
  };
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",45);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.nrows = 45;
  row.npar  =  2;
  row.type  =  5; // log(Qcm)
  // QcmGFRunXV14pp200
  // FitP->Draw("mu:log(y)>>I","(i&&j&&i<=13)/dmu**2","profg")
  // FitP->Draw("mu:log(y)>>O","(i&&j&&i>13)/dmu**2","profgsame")
  for (Int_t i = 0; i < 45; i++) {
    row.idx  = i + 1;
    row.a[0] = rowsGain[i];
    if (i < 13) {
      row.a[1]  = -0.00984444;
    } else {
      row.a[1]  = -0.00698964;
    }
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
