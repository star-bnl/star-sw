TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII109.root; FitP->Draw("mu:TMath::Log2(y)>>O(12,0.9,3.3)","i&&j&&i>13&&abs(mu)<0.2","prof"); O->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5;             
  row.a[0]  =  6.66773e-01;// 
  row.a[1]  = -1.21825e+00;// 
  row.a[2]  =  8.00012e-01;// 
  row.a[3]  = -2.28353e-01;// 
  row.a[4]  =  2.32699e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII109.root; FitP->Draw("mu:TMath::Log2(y)>>I(100,0.1,2.5)","i&&j&&i<=13&&abs(mu)<0.4","prof"); 
  row.nrows = nrows;
  row.npar  = 5; 
  row.a[0]  =  4.78722e-01;//  
  row.a[1]  = -1.38580e+00;// 
  row.a[2]  =  1.27380e+00;// 
  row.a[3]  = -5.47521e-01;// 
  row.a[4]  =  9.00984e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3iTPCCGFRunXVIII109.root; FitP->Draw("mu:TMath::Log2(y)>>X(10,0.5,2.1)","i&&j&&i<=40","prof"); X->Fit("pol3","e")
  row.nrows = nrows;
  row.npar  = 4; 
  row.a[0]  =  7.63937e-02;//  
  row.a[1]  = -1.50099e-01;// 
  row.a[2]  =  9.08454e-02;// 
  row.a[3]  = -1.73117e-02;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
