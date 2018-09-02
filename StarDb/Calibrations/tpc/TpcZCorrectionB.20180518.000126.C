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
  row.max = 209.0;
  row.npar       =            3;// Z3CGFRunXVIII126  
  row.a[0]       =  5.22925e-02;// FitP->Draw("mu:y>>O(45,23,209)","i&&j&&i>13&&y>19&&y<209","prof")
  row.a[1]       = -2.58600e-04;// O->Fit("pol2","e")
  row.a[2]       = -8.79286e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  14.0;
  row.max = 209.0;
  row.npar       =            3;// Z3CGFRunXVIII126
  row.a[0]       =   9.96521e-02;// FitP->Draw("mu:y>>I(50,14,209)","i&&j&&i<=13","prof")
  row.a[1]       =  -7.72880e-04;//
  row.a[2]       =  -6.53098e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  26.0;
  row.max = 209.0;
  row.npar       =            4;// Z3iTPCCGFRunXVIII126; FitP->Draw("mu:y>>xx","i&&j&&i<=40&&y>26&&y<209")
  row.a[0]       =     0.115013;// xx->Fit("pol3","e+rub=0.75","",26,209)
  row.a[1]       =   -0.0019223;// 
  row.a[2]       =  1.26205e-05;
  row.a[3]       = -3.32546e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
