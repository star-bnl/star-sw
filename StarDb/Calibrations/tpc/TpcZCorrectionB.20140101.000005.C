TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  28.0;
  row.max = 210.0;
  row.npar       =          104;// Z3CGFRunXIV05AuAu15Old
  row.a[0]       =  1.20099e-02;// FitP->Draw("mu:y","i&&j&&i>13","prof")
  row.a[1]       = -2.51834e-04;// htemp->Fit("pol3","er","",28,210)
  row.a[2]       =  1.94698e-06;// 
  row.a[3]       = -5.78838e-09;
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  20.0;
  row.max = 210.0;
  row.npar       =          103;// 
  row.a[0]       =  1.03011e-01;// FitP->Draw("mu:y","i&&j&&i<=13","prof")
  row.a[1]       = -9.59081e-04;// htemp->Fit("pol2","er","",20,210)
  row.a[2]       =  1.19180e-06;
 tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
