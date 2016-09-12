TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcTanL",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 1;
  row.nrows = nrows;
  row.min  = -1.00;
  row.max  =  1.40;
  row.type = 300; // correct within  the range
  row.npar = 8; 
  row.a[0] = 6.28022e-02;// TanL3DNNFRunXVIAuAu200p41 FitP->Draw("mu:y","i&&j&&i>13&&abs(mu)<0.1","prof"); htemp->Fit("pol7","er","",-1,1.4)
  row.a[1] =-4.45669e-02;//
  row.a[2] =-3.78373e-01;//
  row.a[3] = 3.60519e-01;//
  row.a[4] = 5.91029e-01;//
  row.a[5] =-5.62948e-01;//
  row.a[6] =-3.39916e-01;//
  row.a[7] = 2.85932e-01;//
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 2;
  row.nrows = nrows;
  row.min  = -2.00;
  row.max  =  1.55;
  row.type = 300; // correct within  the range
  row.npar = 8; 
  row.a[0] = 3.06206e-01;// TanL3DNNFRunXVIAuAu200p41 FitP->Draw("mu:y","i&&j&&i<=13&&abs(mu)<0.4","prof") htemp->Fit("pol7","er","",-2,1.55)
  row.a[1] =-1.77519e-02;//
  row.a[2] =-8.15467e-01;//
  row.a[3] = 2.74177e-03;//
  row.a[4] = 5.15499e-01;//
  row.a[5] = 8.32203e-02;//
  row.a[6] =-1.10729e-01;//
  row.a[7] =-3.22927e-02;//
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
