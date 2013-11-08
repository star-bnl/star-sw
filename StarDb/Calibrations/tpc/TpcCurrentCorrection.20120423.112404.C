TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentGFRunXII04UU193
  row.npar       =            5; // 
  row.min        =           0.;
  row.max        =         0.25;
  row.a[0]       = 1.80337e-02;// 
  row.a[1]       =-5.16555e-01;//
  row.a[2]       = 5.32302e+00;//
  row.a[3]       =-2.62046e+01;//
  row.a[4]       = 4.59286e+01;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentGFRunXII04UU193
  row.npar       =            4; // 
  row.min        =           0.;
  row.max        =         0.45;
  row.a[0]       = 2.60577e-02;//
  row.a[1]       =-1.63737e-01;//
  row.a[2]       =-5.78871e-02;//
  row.a[3]       = 4.29711e-01;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

