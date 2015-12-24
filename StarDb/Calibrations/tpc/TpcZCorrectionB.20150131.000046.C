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
  row.npar       =            4;// Z3CGFRunXIV56AuAu200_P15ib
  row.a[0]       =  4.57225e-02;// FitP->Draw("mu:y>>O(100,26,207)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -4.26996e-04;// O->Fit("pol3","e")
  row.a[2]       =  1.26434e-06;// 
  row.a[3]       = -2.40306e-09;
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  34.0;
  row.max = 207.0;
  row.npar       =            4;// 
  row.a[0]       =  8.23741e-02;// FitP->Draw("mu:y>>I(100,34,207)","i&&j&&i<=13","prof")
  row.a[1]       =  6.04429e-04;// I->Fit("pol3","e")
  row.a[2]       = -1.14717e-05;
  row.a[3]       =  2.68954e-08;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
