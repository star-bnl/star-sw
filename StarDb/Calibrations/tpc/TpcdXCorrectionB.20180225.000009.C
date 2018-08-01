TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII09.root; FitP->Draw("mu:TMath::Log2(y)>>O(21,0.9,3.0)","(i&&j&&i>13&&mu<0.5)","prof"); O->Fit("pol3","em")
  row.nrows = nrows;
  row.min   = 0.90;
  row.max   = 3.00;
  row.npar  = 4;             
  row.a[0]  =  5.09753e-02;// 
  row.a[1]  = -1.21011e-01;// 
  row.a[2]  =  8.87113e-02;// 
  row.a[3]  = -2.41015e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII09.root; FitP->Draw("mu:TMath::Log2(y)>>I(20,0.2.0,2.2)","(i&&j&&i<=13)","prof");  I->Fit("pol4","e");
  row.nrows = nrows;
  row.min   = 0.2;
  row.max   = 2.0;
  row.npar  = 5; 
  row.a[0]  =  4.30521e-01;//  
  row.a[1]  = -1.52406e+00;// 
  row.a[2]  =  1.55000e+00;// 
  row.a[3]  = -7.16005e-01;// 
  row.a[4]  =  1.20512e-01;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
