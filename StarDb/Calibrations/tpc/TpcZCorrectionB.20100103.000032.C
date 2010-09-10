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
  row.npar       =          104;// Z3CGFRunX30P10i_AuAu200_production_FullField,  
  row.a[0]       = -1.24654e-02 -1.22052e-02;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       =  2.01412e-04 +3.60978e-04;// htemp->Fit("pol2","er","",30,210)
  row.a[2]       = -8.24099e-07 -2.75214e-06;//
  row.a[3]       = 	         5.08162e-09;
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  58.0;
  row.max = 208.0;
  row.npar       =          105;// Z3CGFRunX32P10i_AuAu200_production_FullField,  
  row.a[0]       =  7.47240e-02 +3.13031e-02 +7.94808e-02 +9.89502e-02;// 
  row.a[1]       =  3.04549e-04 -3.88073e-04 -3.21934e-03 -1.97799e-03;// htemp->Fit("pol2","er","",40,210)
  row.a[2]       = -5.15092e-06 +1.04529e-06 +4.80006e-05 +1.29184e-05;//
  row.a[3]       =			     -2.79273e-07 -2.76120e-08;
  row.a[4]       =			      5.32432e-10;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
