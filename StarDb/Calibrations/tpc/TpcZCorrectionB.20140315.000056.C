TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  24.0;
  row.max = 208.0;
  row.npar       =          104;// Z3CGFRunXIV56AuAu200_P15ib
  row.a[0]       =  2.69334e-03-1.00172e-02+4.76291e-03;// FitP->Draw("mu:y>>O(20,24,209)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -2.90660e-04+4.26656e-04-2.74483e-04;// O->Fit("pol3","e")
  row.a[2]       =  3.31887e-06-3.56007e-06+2.64215e-06;// 
  row.a[3]       = -9.67061e-09+7.30777e-09-6.28493e-09;
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  45.0;
  row.max = 208.0;
  row.npar       =          104;// 
  row.a[0]       =  1.02919e-01+3.30149e-04-9.58583e-02;// FitP->Draw("mu:y>>I(20,45,208)","i&&j&&i<=13","prof")
  row.a[1]       = -1.72522e-03+2.12299e-03+9.91764e-04;// I->Fit("pol3","e")
  row.a[2]       =  1.11920e-05-2.69594e-05-1.73010e-06;
  row.a[3]       = -3.14400e-08+8.13254e-08-4.48517e-09;

  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
