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
  row.max = 207.0;
  row.npar       =            2;// Z3CGFRunXVIII112  
  row.a[0]       =  4.80275e-02;// FitP->Draw("mu:y>>O(50,24,207)","i&&j&&i>13&&abs(mu)<0.5","prof")
  row.a[1]       = -3.94898e-04;// O->Fit("pol1","e")
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  27.0;
  row.max = 207.0;
  row.npar       =            7;// Z3CGFRunXVIII112
  row.a[0]       =  2.07782e+00;// FitP->Draw("mu:y>>I(45,27,207)","i&&j&&i<=13","prof")
  row.a[1]       = -9.70428e-02;// I->Fit("pol6","e")
  row.a[2]       =  1.89871e-03;
  row.a[3]       = -1.94419e-05;
  row.a[4]       =  1.09240e-07;
  row.a[5]       = -3.19895e-10;
  row.a[6]       =  3.82060e-13;
 tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 208.0;
  row.npar       =            2;// Z3iTPCCGFRunXVIII112
  row.a[0]       =  5.52440e-02;//FitP->Draw("mu:y>>X(90,40,207)","i&&j&&i<=40&&abs(mu)<0.8","prof")
  row.a[1]       = -4.40029e-04;// X->Fit("pol5","e")
  row.a[2]       = ;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
