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
  row.max = 210.0;
  row.npar       =          105;// Z3CGFRunXIII05pp500 + Z3CGFRunXIII10pp500p1+Z3CGFRunXIII13pp500p1
  row.a[0]       =  3.26315e-03 -2.33410e-02+6.49294e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/dmu**2","profg")
  row.a[1]       =  5.74883e-05 +1.06442e-03-1.68525e-04;// htemp->Fit("pol2","er","",25,210)
  row.a[2]       = -5.49567e-07 -1.52591e-05+1.41431e-06;// 
  row.a[3]       =	         8.63250e-08-3.71834e-09;
  row.a[4]       =	        -1.68338e-10;
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 210.0;
  row.npar       =          104;// 
  row.a[0]       =  3.72821e-02+1.65339e-01 -1.11563e-01;// FitP->Draw("mu:y","(i&&j&&i<=13&&dmu<0.02&dmu>5e-3)/(dmu**2)","profg")
  row.a[1]       =  1.76051e-04-2.88110e-03 +2.01990e-03;// htemp->Fit("pol2","er","",40,210)
  row.a[2]       = -2.85921e-06+1.60998e-05 -1.17280e-05;
  row.a[3]       =	       -2.90900e-08 +2.23323e-08;
 tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
