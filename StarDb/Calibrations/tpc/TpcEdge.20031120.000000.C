TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEdge",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows       =       nrows;//
  row.idx         =           1;//
  row.type        =           4;// X = TMath::Sign(TMath::Log(1. - TMath::Abs(x)),x);
  row.npar        =           9;
  row.a[0]	  = 7.37399e-04;//
  row.a[1]	  =-7.07058e-03;// 
  row.a[2]	  = 1.66788e-02;// 
  row.a[3]	  = 2.52524e-03;// 
  row.a[4]	  =-6.83953e-03;// 
  row.a[5]	  =-1.86662e-04;// 
  row.a[6]	  = 9.54329e-04;//
  row.a[7]	  =-1.57500e-05;// 
  row.a[8]	  =-5.00971e-05;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows       =       nrows;
  row.idx         =           2;
  row.type        =           4;// X = TMath::Sign(TMath::Log(1. - TMath::Abs(x)),x);
  row.npar        =           5;//
  row.min         =      -0.925;
  row.max         =       0.925;
  row.a[0]	  = 3.55997e-03;// 
  row.a[1]	  = 1.32092e-03;
  row.a[2]	  = 9.68897e-04;// 
  row.a[3]	  =-1.72779e-04;
  row.a[4]	  =-1.51949e-03;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
