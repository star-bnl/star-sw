TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",2);
  memset(&row,0,tableSet->GetRowSize()); // dXdECGFRunXII05UU1932 + dXdECGFRunXII10UU1932
  row.nrows = 2;
  row.idx   = 1;
  row.npar  = 5;
  row.min   = 0.9;
  row.max   = 2.4;
  row.a[0]  =  5.59398e-02-4.48270e-01;
  row.a[1]  = -4.45143e-02+1.29946e+00;
  row.a[2]  =             -1.34183e+00;
  row.a[3]  =              5.83768e-01;
  row.a[4]  =             -9.05054e-02;
  tableSet->AddAt(&row); // Outer correction versus Log2(dX)
  memset(&row,0,tableSet->GetRowSize()); 
  row.min   = 0.0;
  row.idx   =   2;
  row.npar  =   7;
  row.min   = 0.2;
  row.max   = 1.8;
  row.a[0]  =  1.01994e-01 +2.12801e-02;
  row.a[1]  = -7.24522e-01 -7.73247e-02;
  row.a[2]  =  1.10919e+00 -7.55088e-03;
  row.a[3]  =  3.04417e-01 +2.78686e-02;
  row.a[4]  = -1.71614e+00;
  row.a[5]  =  1.16789e+00;
  row.a[6]  = -2.41899e-01;
tableSet->AddAt(&row); // Inner correction versus Log2(dX) from TpcRS with Heed
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
