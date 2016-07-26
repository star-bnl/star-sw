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
  row.a[0] =-1.26419e-02;// TanL3DCGFRunXVIAuAu200p14 FitP->Draw("mu:y","i&&j&&i>13&&abs(mu)<0.1","prof"); htemp->Fit("pol7","er","",-0.1,1.4)
  row.a[1] =-6.12384e-02;//
  row.a[2] = 8.60546e-01;//
  row.a[3] =-3.93357e+00;//
  row.a[4] = 8.87071e+00;//
  row.a[5] =-1.01898e+01;//
  row.a[6] = 5.68282e+00;//
  row.a[7] =-1.22012e+00;//
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 2;
  row.nrows = nrows;
  row.min  = -1.00;
  row.max  =  1.55;
  row.type = 300; // correct within  the range
  row.npar = 8; 
  row.a[0] = 9.05460e-02;// TanL3DCGFRunXVIAuAu200p14 FitP->Draw("mu:y","i&&j&&i<=13&&abs(mu)<0.15","prof"); htemp->Fit("pol7","er","",-1.,1.55)
  row.a[1] =-7.34254e-02;//
  row.a[2] =-3.38195e-01;//
  row.a[3] = 1.77845e-01;//
  row.a[4] =-3.21704e-02;//
  row.a[5] = 5.58657e-02;//
  row.a[6] = 1.81290e-01;//
  row.a[7] =-1.28761e-01;//
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
