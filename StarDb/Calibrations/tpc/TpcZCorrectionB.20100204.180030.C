TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  30.0;
  row.max = 208.0;
  row.npar       =          103;// Z3CGFRunX30P10i_AuAu200_production_ReversedFullField,  
  row.a[0]       = -1.24654e-02 -4.11214e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       =  2.01412e-04 +1.28991e-04;// htemp->Fit("pol2","er","",30,210)
  row.a[2]       = -8.24099e-07 -7.93267e-07;//
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  58.0;
  row.max = 208.0;
  row.npar       =          103;// Z3CGFRunX30P10i_AuAu200_production_ReversedFullField,  
  row.a[0]       =  7.47240e-02 +3.13031e-02 -2.21526e-02;// FitP->Draw("mu:y","(i&&j&&i<=13)/(dmu**2)","profg")
  row.a[1]       =  3.04549e-04 -3.88073e-04 +7.30830e-04;// htemp->Fit("pol2","er","",40,210)
  row.a[2]       = -5.15092e-06 +1.04529e-06 -3.78512e-06;//
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
