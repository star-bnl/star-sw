TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",50);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.0;
  row.max = 210.0;
  row.npar =             3;//  FitP->SetMarkerColor(1); FitP->Draw("mu:y>> O(105,0,210)","i&&j&&abs(x)>40.5","prof");
  row.a[0] =   6.65241e-02;
  row.a[1] =  -4.19794e-04;
  row.a[2] =  -8.68999e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 210.0;
  row.npar =             4;//  FitP->SetMarkerColor(2); FitP->Draw("mu:y>> I(105,0,210)","i&&j&&abs(x)<40.5","prof");
  row.a[0] =   1.08222e-01;
  row.a[1] =   1.69985e-03;
  row.a[2] =  -3.19109e-05;
  row.a[3] =   8.94157e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t i = nrows; i < 50; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
