TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =          1; // Z3GFHist111P06id_calib_pp_?
  row.nrows	 =          2; //
  row.npar	 =        110; //
  row.min        =         20;
  row.max        =        210;
  row.a[0]	 =   -5.21925e-01;//
  row.a[1]	 =    6.34902e-02;//
  row.a[2]	 =   -2.83950e-03;//
  row.a[3]	 =    6.83466e-05;//
  row.a[4]	 =   -9.93282e-07;//
  row.a[5]	 =    9.11980e-09;//
  row.a[6]	 =   -5.32348e-11;//
  row.a[7]	 =    1.91424e-13;//
  row.a[8]	 =   -3.86246e-16;//
  row.a[9]	 =    3.34302e-19;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx	 =          2; // ;  Phi3GFHist009P05id
  row.nrows	 =          2; // ;
  row.npar	 =        102; // Cut
  row.min        =         20;
  row.max        =        210;
  row.a[0]	 =    8.85759e-02;//
  row.a[1]	 =   -4.32651e-04;//
  row.a[2]	 =    7.20774e-07;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
