TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  23.0;
  row.max = 210.0;
  row.npar       =          103;// Z3CGFRunXII07UU193.root + Z3CGFRunXII10UU1932 + Z3CGFRunXII12UU193
  row.a[0]       =  1.42766e-02+5.73250e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       = -4.13921e-05-5.24401e-05;// htemp->Fit("pol2","er","",23,210)
  row.a[2]       = -4.82813e-07+4.61772e-08;// 
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 210.0;
  row.npar       =          106;// 
  row.a[0]       =  3.99834e-01+2.69964e-01-3.52519e-01;// FitP->Draw("mu:y","(i&&j&&i<=13)/(dmu**2)","profg")
  row.a[1]       = -1.29407e-02-9.87636e-03+1.60059e-02;// htemp->Fit("pol5","er","",40,210)
  row.a[2]       =  1.86564e-04+1.45412e-04-2.56788e-04;
  row.a[3]       = -1.36976e-06-1.06793e-06+1.96399e-06;
  row.a[4]       =  4.94939e-09+3.87293e-09-7.32860e-09;
  row.a[5]       = -7.09634e-12-5.52185e-12+1.07022e-11;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
