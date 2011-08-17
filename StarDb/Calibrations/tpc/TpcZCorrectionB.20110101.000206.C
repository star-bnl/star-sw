TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 210.0;
  row.npar       =          102;// Z3CGFRunXI35pp500dev_calib_pp500_production_2011_ReversedFullField
  row.a[0]       = -5.74030e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       =  4.56995e-05;// htemp->Fit("pol1","er","",20,210)
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  24.0;
  row.max = 210.0;
  row.npar       =           106;// Z3CGFRunXI36pp500dev_calib_pp500_production_2011_ReversedFullField
  row.a[0]       =   6.07058e-02+3.76326e-01;// FitP->Draw("mu:y","(i&&j&&i<=13)/(dmu**2)","profg")
  row.a[1]       =  -4.25280e-04-1.66417e-02;// htemp->Fit("pol1","e")
  row.a[2]       =  	         2.77925e-04;
  row.a[3]       =	        -2.21233e-06;
  row.a[4]       =	         8.47482e-09;
  row.a[5]       =	        -1.25924e-11;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
