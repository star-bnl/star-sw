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
  row.max = 208.0;
  row.npar       =            4;// Z3CGFRunXVIII05 
  row.a[0]       =  1.05921e-02;// FitP->Draw("mu:y>>O(45,26,208)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       =  8.15304e-06;// O->Fit("pol1","e")
  row.a[2]       = -3.57355e-07;// O->Fit("pol1","e")
  row.a[3]       = -3.00179e-09;// O->Fit("pol1","e")
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 208.0;
  row.npar       =            5;// 
  row.a[0]       =  1.99540e-01 ;// FitP->Draw("mu:y>>I(45,26,208)","i&&j&&i<=13","prof")
  row.a[1]       = -5.95610e-04 ;// I->Fit("pol4","e")
  row.a[2]       = -4.85876e-05 ;
  row.a[3]       =  4.47924e-07 ;
  row.a[4]       = -1.06155e-09 ;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
