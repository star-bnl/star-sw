TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrectionB",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;  //dX3NNFRunXV41pp200.root  FitP->Draw("mu:TMath::Log2(y)","i&&j&&i>13","prof")
  row.nrows = 2;
  row.npar  = 2; 	     
  row.a[0]  =  1.63193e-01;// 
  row.a[1]  = -1.27034e-01;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;   // dX3NNFRunXV41pp200.root  FitP->Draw("mu:TMath::Log2(y)","i&&j&&i<=13","prof")
  row.nrows = 2;
  row.npar  = 5; 
  row.a[0]  =  3.21264e-01;//  
  row.a[1]  = -1.24141e+00;// 
  row.a[2]  =  1.51078e+00;// 
  row.a[3]  = -8.36111e-01;// 
  row.a[4]  =  1.71375e-01;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
