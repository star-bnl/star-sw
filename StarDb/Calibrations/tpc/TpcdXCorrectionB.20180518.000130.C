TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII130.root; FitP->Draw("mu:TMath::Log2(y)>>O(26,0.8,3.4)","i&&j&&i>13&&abs(mu)<0.2","prof"); O->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5;   
  row.min   = 0.8;
  row.max   = 3.4;          
  row.a[0]  =  6.99091e-01;// 
  row.a[1]  = -1.47921e+00;// 
  row.a[2]  =  1.16511e+00;// 
  row.a[3]  = -4.04674e-01;// 
  row.a[4]  =  5.00816e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII130.root; FitP->Draw("mu:TMath::Log2(y)>>I(100,0.1,2.5)","i&&j&&i<=13&&abs(mu)<0.4","prof"); I->Fit("pol4","e")
  row.nrows = nrows; 
  row.npar  = 5; 
  row.min   = 0.1;
  row.max   = 2.5;
  row.a[0]  =   5.84325e-01;// 
  row.a[1]  =  -1.84609e+00;//  
  row.a[2]  =   1.92659e+00;// 
  row.a[3]  =  -8.92722e-01;// 
  row.a[4]  =   1.50765e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3iTPCCGFRunXVIII130.root; FitP->Draw("mu:TMath::Log2(y)>>X(20,0.5,2.5)","i&&j&&i<=40","prof");
  row.nrows = nrows;
  row.npar  = 8; 
  row.min   = 0.5;
  row.max   = 2.5;
  row.a[0]  =  1.06734e+00;//  
  row.a[1]  = -4.20634e+00;// 
  row.a[2]  =  6.18837e+00;// 
  row.a[3]  = -3.68102e+00;// 
  row.a[4]  =  1.04478e-02;// 
  row.a[5]  =  9.89182e-01;// 
  row.a[6]  = -4.18972e-01;// 
  row.a[7]  =  5.49309e-02;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
