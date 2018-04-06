TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFdEdx10.root; FitP->Draw("mu:TMath::Log2(y)>>O(100,0.9,3.1)","(i&&j&&i>13&&mu<0.1)","prof"); O->Fit("pol7","e")
  row.nrows = nrows;
  row.min   = 0.95;
  row.max   = 1.90;
  row.npar  = 8;             
  row.a[0]  =  1.78039e+01;// 
  row.a[1]  = -7.33909e+01;// 
  row.a[2]  =  1.26472e+02;// 
  row.a[3]  = -1.17986e+02;// 
  row.a[4]  =  6.43381e+01;// 
  row.a[5]  = -2.05257e+01;// 
  row.a[6]  =  3.55341e+00;// 
  row.a[7]  = -2.58077e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFdEdx10.root; FitP->Draw("mu:TMath::Log2(y)>>I(100,0.0,2.5)","(i&&j&&i<=13)","prof"); I->Fit("pol4","e")
  row.nrows = nrows;
  row.min   = 0.0;
  row.max   = 2.5;
  row.npar  = 5; 
  row.a[0]  =  2.80598e-01;//  
  row.a[1]  = -1.11443e+00;// 
  row.a[2]  =  1.24339e+00;// 
  row.a[3]  = -5.73302e-01;// 
  row.a[4]  =  9.16752e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3NNFRunXVIAuAu200p36.root   FitP->Draw("mu:TMath::Log2(y)>>I(17,0.3,2.0)","(i&&j&&i<=13)","prof"); iTPC->Fit("pol1")
  row.nrows = nrows;
  row.min   = 0.5;
  row.max   = 2.5;
  row.npar  = 5; 
  row.a[0]  = -1.22167e-01;//  
  row.a[1]  =  9.97059e-02;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
