TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  25.0;
  row.max = 209.0;
  row.npar       =            3;//  Z3iTPCCGFdEdx057p4.root
  row.a[0]       =  1.66532e-02;// FitP->Draw("mu:y>>O(85,25,209)","i&&j&&i>40","prof")
  row.a[1]       = -3.76436e-05;// O->Fit("pol4","e")
  row.a[2]       = -7.14768e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  14.0;
  row.max = 209.0;
  row.npar       =            5;// Z3CGFRunXVIII126
  row.a[0]       = -4.19187e-02;// FitP->Draw("mu:y>>I(94,23,209)","i&&j&&i<=40","prof")
  row.a[1]       =  5.99752e-03;// I->Fit("pol4","e")
  row.a[2]       = -9.60629e-05;
  row.a[3]       =  5.11020e-07;
  row.a[4]       = -9.04341e-10;
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
