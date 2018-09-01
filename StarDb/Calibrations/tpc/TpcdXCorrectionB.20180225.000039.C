TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  // dX3CGFRunXVIII27.root; FitP->Draw("mu:TMath::Log2(y)>>O","i&&j&&i>13","prof"); O->Fit("pol5","e")
  row.nrows = nrows;
  row.min   = 0.9;
  row.max   = 2.3;
  row.npar  = 6;             
  row.a[0]  =  1.28245e-01+8.04218e+00;// 
  row.a[1]  = -4.31331e-01-2.70082e+01;// 
  row.a[2]  =  4.52082e-01+3.56120e+01;// 
  row.a[3]  = -1.86832e-01-2.30418e+01;// 
  row.a[4]  =  2.49101e-02+7.31216e+00;// 
  row.a[5]  =             -9.10937e-01;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3CGFRunXVIII27.root;  FitP->Draw("mu:TMath::Log2(y)>>I","i&&j&&i<=13","prof");  I->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5; 
  row.a[0]  =  5.03642e-01+1.11905e-01;//  
  row.a[1]  = -1.82838e+00-4.19133e-01;// 
  row.a[2]  =  2.00206e+00+5.57302e-01;// 
  row.a[3]  = -9.88223e-01-2.92300e-01;// 
  row.a[4]  =  1.81129e-01+4.91906e-02;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;   //  FitP->Draw("mu:TMath::Log2(y)>>X","i&&j&&i<=40","prof");  X->Fit("pol4","e")
  row.nrows = nrows;
  row.npar  = 5; 
  row.a[0]  =  5.03642e-01-2.42453e-01;//  
  row.a[1]  = -1.82838e+00+8.27086e-01;// 
  row.a[2]  =  2.00206e+00-1.02186e+00;// 
  row.a[3]  = -9.88223e-01+5.47136e-01;// 
  row.a[4]  =  1.81129e-01-1.07647e-01;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
