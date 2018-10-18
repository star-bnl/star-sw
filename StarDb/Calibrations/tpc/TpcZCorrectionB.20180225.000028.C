TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  24.0;
  row.max = 208.0;
  row.npar       =            4;// Z3CGFRunXVIII28 
  row.a[0]       =  1.53338e-02-1.08029e-02;// FitP->Draw("mu:y>>O(50,24,208)","i&&j&&i>13","prof")
  row.a[1]       = -1.76768e-04+7.48107e-05;// O->Fit("pol3","e")
  row.a[2]       =  1.31498e-06            ;//
  row.a[3]       = -7.32449e-09 	   ;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 208.0;
  row.npar       =            4;// Z3CGFRunXVIII28 
  row.a[0]       =  9.76725e-02+4.71198e-03  -0.0689513;//  FitP->Draw("mu:y>>I(50,20,208)","i&&j&&i<=13","prof")
  row.a[1]       = -2.28989e-04             +0.00143917;// I->Fit("pol3","e") 
  row.a[2]       = -3.78404e-06		   -8.16351e-06;// 	II->Fit("pol3","e+rob=0.75","",20,220)	      
  row.a[3]       =  6.68089e-09		   +6.96658e-09;                      
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 208.0;
  row.npar       =            4;// Z3iTPCCGFRunXVIII28 
  row.a[0]       =  9.76725e-02;//  FitP->Draw("mu:y>>I(50,20,208)","i&&j&&i<=13","prof")
  row.a[1]       = -2.28989e-04;// I->Fit("pol3","e")
  row.a[2]       = -3.78404e-06;
  row.a[3]       =  6.68089e-09;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
