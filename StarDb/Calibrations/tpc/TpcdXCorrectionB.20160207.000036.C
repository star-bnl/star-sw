TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  //dX3NNFRunXVIAuAu200p36.root  FitP->Draw("mu:TMath::Log2(y)>>O(22,0.95,1.9)","(i&&j&&i>13)","prof")
  row.nrows = 2;
  row.min   = 0.95;
  row.max   = 1.90;
  row.npar  = 5;             
  row.a[0]  =  7.95352e+00;// 
  row.a[1]  = -2.19153e+01;// 
  row.a[2]  =  2.23874e+01;// 
  row.a[3]  = -1.00127e+01;// 
  row.a[4]  =  1.64162e+00;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3NNFRunXVIAuAu200p36.root   FitP->Draw("mu:TMath::Log2(y)>>I(17,0.3,2.0)","(i&&j&&i<=13)","prof")
  row.nrows = 2;
  row.min   = 0.3;
  row.max   = 2.0;
  row.npar  = 5; 
  row.a[0]  =  1.33709e+00;//  
  row.a[1]  = -5.46051e+00;// 
  row.a[2]  =  7.49339e+00;// 
  row.a[3]  = -4.48933e+00;// 
  row.a[4]  =  9.64855e-01;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
