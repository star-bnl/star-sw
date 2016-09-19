TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  26.0;
  row.max = 207.0;
  row.npar       =            4;// Z3CGFRunXVIAuAu200p102
  row.a[0]       =  5.00045e-02-1.94242e-04;// FitP->Draw("mu:y>>O(100,26,207)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -9.19946e-04+1.30038e-04;// O->Fit("pol3","e")
  row.a[2]       =  7.88795e-06-1.56136e-06;// 
  row.a[3]       = -2.12427e-08+4.37781e-09;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  47.0;
  row.max = 207.0;
  row.npar       =            4;// 
  row.a[0]       =  4.40656e-03+2.97015e-01;//  FitP->Draw("mu:y>>I(100,47,207)","i&&j&&i<=13","prof")
  row.a[1]       =  1.25421e-03-5.53135e-03;// I->Fit("pol3","e")
  row.a[2]       = -1.30037e-05+3.49183e-05;
  row.a[3]       =  3.34273e-08-7.59519e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
