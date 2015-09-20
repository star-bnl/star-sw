TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentGFRunXV04pp200
  row.npar       =            2; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0]       =  1.35905e-02;// 
  row.a[1]       = -8.93308e-02;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentGFRunXV04pp200
  row.npar       =            2; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0]       =  3.19999e-02;
  row.a[1]       = -1.44054e-01;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

