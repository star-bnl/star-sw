TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGF102RC4.root; FitP->Draw("mu:TMath::Log2(y)>>O(11,0.9,1.8)","(i&&j&&i>13&&mu<0.1)","prof"); O->Fit("pol4","e")
  row.nrows = nrows;
  row.min   = 0.90;
  row.max   = 1.80;
  row.npar  = 5;             
  row.a[0]  =  3.25198e+00;// 
  row.a[1]  = -9.89692e+00;// 
  row.a[2]  =  1.10305e+01;// 
  row.a[3]  = -5.33816e+00;// 
  row.a[4]  =  9.46326e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGF102RC4.root; FitP->Draw("mu:TMath::Log2(y)>>I(18,0.2,2.0)","(i&&j&&i<=13)","prof"); I->Fit("pol5","e")
  row.nrows = nrows;
  row.min   = 0.2;
  row.max   = 2.0;
  row.npar  = 6; 
  row.a[0]  =  2.14845e-01;//  
  row.a[1]  =  2.85110e-02;// 
  row.a[2]  = -2.91838e+00;// 
  row.a[3]  =  5.56181e+00;// 
  row.a[4]  = -3.93408e+00;// 
  row.a[5]  =  9.62501e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3iTPCCGF102RC4;  FitP->Draw("mu:TMath::Log2(y)>>X(20,0.6,2.6)","(i&&j&&i<=40)&&abs(mu)<0.2","prof");  X->Fit("pol3","e")
  row.nrows = nrows;
  row.npar  = 4; 
  row.a[0]  = -4.98217e-02;//  
  row.a[1]  =  5.12404e-02;// 
  row.a[2]  = -2.05932e-03;// 
  row.a[3]  = -4.59954e-03;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
