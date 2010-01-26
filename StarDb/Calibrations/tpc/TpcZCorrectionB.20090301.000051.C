TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           1; // Z3CGFRunIX51P09if
  row.nrows	 =           2; //
  row.npar	 =         103; //
  row.min        =          30;
  row.max        =         210;
  row.a[0]	 = 5.06523e-04; // 
  row.a[1]	 = 8.39157e-05; //
  row.a[2]	 =-7.62381e-07; //
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           2; // 
  row.nrows	 =           2; // 
  row.npar	 =         107; //
  row.min        =          40;
  row.max        =         210;
  row.a[0]	 =-7.08651e-01; // 
  row.a[1]	 = 3.58825e-02; //
  row.a[2]	 =-7.24557e-04; //
  row.a[3]	 = 7.62605e-06; // 
  row.a[4]	 =-4.41989e-08; //
  row.a[5]	 = 1.33529e-10; //
  row.a[6]	 =-1.64469e-13; // 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
