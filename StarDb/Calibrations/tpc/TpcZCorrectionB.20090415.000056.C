TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 5;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           1; // Z3CGFRunIX53P09igBC
  row.nrows	 =       nrows; //
  row.npar	 =         108; //
  row.min        =          30;
  row.max        =         210;
  row.a[0]	 = 4.21632e-02 -1.49961e-03; // 
  row.a[1]	 =-3.22610e-03; //
  row.a[2]	 = 1.04146e-04; //
  row.a[3]	 =-1.76895e-06; //
  row.a[4]	 = 1.70259e-08; //
  row.a[5]	 =-9.35034e-11; //
  row.a[6]	 = 2.72656e-13; //
  row.a[7]	 =-3.27443e-16; //
  tableSet->AddAt(&row);        // Outer 1
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           2; // 
  row.nrows	 =       nrows; // 
  row.npar	 =         108; //
  row.min        =          25;
  row.max        =         210;
  row.a[0]	 = 4.57753e-01+3.30940e-03; // Z3CGFRunIX56P09igBC
  row.a[1]	 =-2.83340e-02-2.60548e-05; //
  row.a[2]	 = 7.53281e-04; //
  row.a[3]	 =-1.07809e-05; // 
  row.a[4]	 = 8.90631e-08; //
  row.a[5]	 =-4.24740e-10; //
  row.a[6]	 = 1.08508e-12; // 
  row.a[7]	 =-1.15015e-15; //
  tableSet->AddAt(&row);        // Inner 1
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           3; // Z3CGFRunIX53P09igBC
  row.nrows	 =       nrows; //
  row.npar	 =         104; //
  row.min        =          15;
  row.max        =          30;
  row.a[0]	 = 9.03792e-03-4.91396e+00; // 
  row.a[1]	 = 7.64195e-04+5.55089e-01; //
  row.a[2]	 = 1.58852e-05-2.04351e-02; //
  row.a[3]       =             2.46384e-04;
  tableSet->AddAt(&row);        // Outer 2
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           4; // 
  row.nrows	 =       nrows; // 
  row.npar	 =         104; //
  row.min        =           0;
  row.max        =          25;
  row.a[0]	 = 7.78248e-02; // 
  row.a[1]	 =-1.69059e-03; //
  row.a[2]	 = 1.30618e-05; //
  row.a[3]	 =-3.49897e-08; // 
  tableSet->AddAt(&row);        // Inner 2
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =           5; // Z3CGFRunIX53P09igBC
  row.nrows	 =       nrows; //
  row.npar	 =         101; //
  row.min        =           0;
  row.max        =          15;
  row.a[0]	 = 6.17684e-02; // 
  tableSet->AddAt(&row);        // Outer 3
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
