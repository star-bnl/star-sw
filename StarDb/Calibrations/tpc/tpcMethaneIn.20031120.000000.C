TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 1;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcMethaneIn",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
#if 0
  row.idx       = 1;
  row.nrows       = nrows;
  row.npar        =            2;       //percentMethaneInPCGFHist843P04ifAuAu200
  row.a[0]	  =  2.19558e-01;// 2.50502e-01;
  row.a[1]	  = -2.17270e-02;
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
