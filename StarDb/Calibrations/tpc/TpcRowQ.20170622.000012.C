TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Double_t rowsGain[45] = {
      -0.10719,  -0.10074,  -0.10168,  -0.09698,  -0.09621,
      -0.09015,  -0.09162,  -0.09942,  -0.09717,  -0.09765,
      -0.10212,  -0.11200,  -0.13597,   0.12815,   0.13361,
       0.12824,   0.12886,   0.13247,   0.13733,   0.13081,
       0.13780,   0.13473,   0.13008,   0.12836,   0.12310,
       0.12697,   0.13031,   0.12348,   0.12281,   0.12096,
       0.11975,   0.10688,   0.11527,   0.10797,   0.10904,
       0.09795,   0.09315,   0.09172,   0.09273,   0.08306,
       0.07979,   0.07264,   0.06679,   0.04851,   0.00671
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
