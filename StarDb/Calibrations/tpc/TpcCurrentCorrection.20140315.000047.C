TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentGFRunXIV47AuAu200
  row.npar       =            2; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0]       = 6.32172e-03;// 
  row.a[1]       =-2.09787e-02;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentGFRunXIV47AuAu200
  row.npar       =            3; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0] =         3.85202e-03;
  row.a[1] =         3.28698e-02;
  row.a[2] =        -7.76025e-02;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

