TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  23.0;
  row.max = 207.0;
  row.npar       =            2;// Z3CGFRunXVIII105  
  row.a[0]       =  4.48274e-02;// FitP->Draw("mu:y>>O(50,23,207)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -3.63824e-04;// O->Fit("pol1","e")
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  23.0;
  row.max = 207.0;
  row.npar       =            7;// Z3CGFRunXVIII105
  row.a[0]       =  1.64505e+00 ;// FitP->Draw("mu:y>>I(100,23,207)","i&&j&&i<=13","prof")
  row.a[1]       = -7.33017e-02 ;// I->Fit("pol6","e")
  row.a[2]       =  1.38702e-03 ;
  row.a[3]       = -1.38615e-05 ;
  row.a[4]       =  7.64845e-08 ;
  row.a[5]       = -2.21036e-10 ;
  row.a[6]       =  2.61546e-13 ;
 tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 208.0;
  row.npar       =            6;// Z3iTPCCGFRunXVIII105
  row.a[0]       =   2.08300e-01;// FitP->Draw("mu:y>>X(45,26,207)","i&&j&&i<=40","prof")
  row.a[1]       =  -6.91004e-03;// X->Fit("pol5","e")
  row.a[2]       =   1.11790e-04;
  row.a[3]       =  -9.28394e-07;
  row.a[4]       =   3.73387e-09;
  row.a[5]       =  -5.83606e-12;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
