TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           1; // Z3CGFRunIX38P09ifD + Z3CGFRunIX39P09if
  row.nrows	 =           2; //
  row.npar	 =           3; //
  row.a[0]	 = 1.51186e-02 -1.12129e-02; // 
  row.a[1]	 =-9.19888e-05 +1.29085e-04; //
  row.a[2]	 =             -5.90784e-07; //
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           2; // 
  row.nrows	 =           2; // 
  row.npar	 =           3; //
  row.a[0]	 = 4.13596e-02 -3.87953e-02; // 
  row.a[1]	 =-2.25486e-04 +5.66174e-04; //
  row.a[2]	 =             -2.28317e-06; //
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
