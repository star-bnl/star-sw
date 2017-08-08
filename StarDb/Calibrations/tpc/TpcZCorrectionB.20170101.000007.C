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
  row.npar       =            2;// Z3CGFRunXVII07
  row.a[0]       =  2.03638e-02;// FitP->Draw("mu:y>>O(45,26,208)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -1.57694e-04;// O->Fit("pol1","e")
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 208.0;
  row.npar       =            3;// 
  row.a[0]       =   2.88077e-01;// FitP->Draw("mu:y>>I(100,20,208)","i&&j&&i<=13","prof")
  row.a[1]       =  -2.80991e-03;// I->Fit("pol2","e")
  row.a[2]       =   5.91905e-06;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
