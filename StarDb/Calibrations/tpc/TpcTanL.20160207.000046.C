TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcTanL",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 1;
  row.nrows = nrows;
  row.min  =  0.00;
  row.max  =  1.50;
  row.type = 300; // correct within  the range
  row.npar = 7; 
  row.a[0] = 6.12639e-02;// TanL3DNNFRunXVIAuAu200p46; FitP->Draw("mu:y>>o","(i&&j&&i>13&&mu<0.1&&mu>-0.08&&dmu<0.005&&dmu>1e-4)/dmu**2","profg"); o->Fit("pol6","er","",-0.1,1.8)
  row.a[1] = 1.89870e-02;//
  row.a[2] =-6.10033e-01;//
  row.a[3] = 1.15098e+00;//
  row.a[4] =-8.59369e-01;//
  row.a[5] = 2.29124e-01;//
  row.a[6] =-4.41945e-03;//
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 2;
  row.nrows = nrows;
  row.min  = -1.00;
  row.max  =  1.0;
  row.type = 300; // correct within  the range
  row.npar = 6; 
  row.a[0] = 3.32653e-01;// TanL3DNNFRunXVIAuAu200p46; FitP->Draw("mu:y>>I","(i&&j&&i<=13&&abs(mu)<0.4)/dmu**2","profg"); I->Fit("pol5","er+rob=0.75","",-1.0,1.0)
  row.a[1] =-4.51781e-02;//
  row.a[2] =-7.26917e-01;//
  row.a[3] = 2.99627e-02;//
  row.a[4] = 2.63000e-01;//
  row.a[5] =-1.70552e-02;//
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
