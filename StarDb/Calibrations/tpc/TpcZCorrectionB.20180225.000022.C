TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 3;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  26.0;
  row.max = 208.0;
  row.npar       =            4;// Z3CGFRunXVII07
  row.a[0]       =  1.97291e-02;// FitP->Draw("mu:y>>O(45,26,208)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -4.82398e-04;// O->Fit("pol1","e")
  row.a[2]       =  3.71172e-06;// O->Fit("pol1","e")
  row.a[3]       = -9.96651e-09;// O->Fit("pol1","e")
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 208.0;
  row.npar       =            5;// 
  row.a[0]       =   3.88670e-02;// FitP->Draw("mu:y>>I(100,20,208)","i&&j&&i<=13","prof")
  row.a[1]       =   3.23823e-03;// I->Fit("pol4","e")
  row.a[2]       =  -5.92377e-05;
  row.a[3]       =   3.30492e-07;
  row.a[4]       =  -6.21784e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  26.0;
  row.max = 210.0;
  row.min =  26.0;
  row.max = 208.0;
  row.npar       =             4;// Z3iTPCCGFdEdx22.root
  row.a[0]       =   1.80691e-01;// FitP->Draw("mu:y>>X(100,26,208)","i&&j&&i<=40","prof")
  row.a[1]       =  -3.56551e-03;// X->Fit("pol3","e")
  row.a[2]       =   2.42709e-05;
  row.a[3]       =  -5.80965e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
