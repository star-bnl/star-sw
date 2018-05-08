TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFdEdx19.root; FitP->Draw("mu:TMath::Log2(y)>>O(11,0.9,1.8)","(i&&j&&i>13&&mu<0.1)","prof"); O->Fit("pol4","e")
  row.nrows = nrows;
  row.min   = 0.90;
  row.max   = 1.80;
  row.npar  = 5;             
  row.a[0]  =  2.03122e+00;// 
  row.a[1]  = -6.15814e+00;// 
  row.a[2]  =  6.91791e+00;// 
  row.a[3]  = -3.40835e+00;// 
  row.a[4]  =  6.19066e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFdEdx19.root; FitP->Draw("mu:TMath::Log2(y)>>I(18,0.2,2.0)","(i&&j&&i<=13)","prof"); I->Fit("pol4","e")
  row.nrows = nrows;
  row.min   = 0.2;
  row.max   = 2.0;
  row.npar  = 5; 
  row.a[0]  =  3.30793e-01;//  
  row.a[1]  = -1.44159e+00;// 
  row.a[2]  =  1.92540e+00;// 
  row.a[3]  = -1.08563e+00;// 
  row.a[4]  =  2.18082e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3iTPCCGFdEdx19;  FitP->Draw("mu:TMath::Log2(y)","(i&&j&&i<=40&&abs(mu)<0.1)","prof");  htemp->Fit("pol3","e")
  row.nrows = nrows;
  row.npar  = 4; 
  row.a[0]  = -3.02687e-01;//  
  row.a[1]  =  5.34096e-01;// 
  row.a[2]  = -2.80228e-01;// 
  row.a[3]  =  4.67558e-02;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
