TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeBucketCor",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; // TpcRS_2012_UU193
  row.idx        =            1; // BO_1->Fit("pol1","er","",0, 19)
  row.npar       =            2; // 
  row.a[0]       =  1.60275e-01; 
  row.a[1]       = -2.58727e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx        =            1; // BI_1->Fit("pol1","er","",0, 19)
  row.npar       =            2; // 
  row.a[0]       =  1.47797e-01; 
  row.a[1]       = -2.84499e-02;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
