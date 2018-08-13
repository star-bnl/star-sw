TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII19.root; >Draw("mu:TMath::Log2(y)>>O(21,0.9,3)","i&&j&&i>13&&abs(mu)<0.2","prof");   O->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5;             
  row.a[0]  =  1.28245e-01;// 
  row.a[1]  = -4.31331e-01;// 
  row.a[2]  =  4.52082e-01;// 
  row.a[3]  = -1.86832e-01;// 
  row.a[4]  =  2.49101e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII19.root; FitP->Draw("mu:TMath::Log2(y)>>I(21,0.1,2.2)","i&&j&&i<=13&&abs(mu)<1&&abs(mu)<0.2","prof"); I->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5; 
  row.a[0]  =  5.03642e-01;//  
  row.a[1]  = -1.82838e+00;// 
  row.a[2]  =  2.00206e+00;// 
  row.a[3]  = -9.88223e-01;// 
  row.a[4]  =  1.81129e-01;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
