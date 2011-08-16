TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  24.0;
  row.max = 210.0;
  row.npar       =          102;// Z3CGFRunXI35AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.a[0]       =  5.01554e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       = -4.07974e-05;// htemp->Fit("pol2","er","",30,210)
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  24.0;
  row.max = 210.0;
  row.npar       =          105;// Z3CGFRunXI36AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.a[0]       =  8.87398e-02 +1.50468e-01;// FitP->Draw("mu:y","(i&&j&&i<=13)/(dmu**2)","profg")
  row.a[1]       = -6.42919e-04 -4.42725e-03;// htemp->Fit("pol2","er","",40,210)
  row.a[2]       =	         4.66580e-05;
  row.a[3]       =	        -2.08424e-07;
  row.a[4]       =	         3.33715e-10;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
