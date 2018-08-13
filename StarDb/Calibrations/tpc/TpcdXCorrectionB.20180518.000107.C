TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII103.root; FitP->Draw("mu:TMath::Log2(y)>>O(17,0.9,3.3)","(i&&j&&i>13)&&mu>-0.3&&mu<0.1","prof"); O->Fit("pol3","e");
  row.nrows = nrows;
  row.min   = 0.90;
  row.max   = 2.00;
  row.npar  = 5;             
  row.a[0]  =  3.07542e-01+7.14397e-01;// 
  row.a[1]  = -4.42855e-01-1.96728e+00;// 
  row.a[2]  =  2.01890e-01+2.04179e+00;// 
  row.a[3]  = -3.22231e-02-9.44339e-01;// 
  row.a[4]  =              1.63401e-01;// 
 tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII103.root; FitP->Draw("mu:TMath::Log2(y)>>I(25,0.2,2.7)","(i&&j&&i<=13)&&abs(mu)<0.4","prof"); I->Fit("pol4","e");
  row.nrows = nrows;
  row.min   = 0.2;
  row.max   = 1.7;
  row.npar  = 5; 
  row.a[0]  =  4.76976e-01-3.34774e-02;//  
  row.a[1]  = -1.33035e+00+1.99798e-02;// 
  row.a[2]  =  1.13431e+00+6.89258e-02;// 
  row.a[3]  = -4.46882e-01-3.87242e-02;// 
  row.a[4]  =  6.75498e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   // dX3iTPCCGFRunXVIII103.root; FitP->Draw("mu:TMath::Log2(y)>>X(25,0.6,3.1)","i&&j&&abs(mu)<0.3&&i<=40","prof"); X->Fit("pol3","e")
  row.nrows = nrows;
  row.min   = 0.6;
  row.max   = 3,1;
  row.npar  = 4; 
  row.a[0]  =  6.10661e-03;//  
  row.a[1]  =  1.80755e-02;// 
  row.a[2]  = -1.13538e-02;// 
  row.a[3]  =  2.03095e-04;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
