TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =  1;
#if 0
  row.npar       =  3;           // Z3CGFHist848P04ifAuAu200
  row.min        = 26.;
  row.max        = 210.;
  row.a[0]	 =  1.21143e-02;//
  row.a[1]	 = -1.40543e-04;//
  row.a[2]	 =  4.33903e-07;//
#if 0
  row.nrows      = nrows;
  row.idx        =  1;
  row.npar       =  3;           // Z3GFHist841P04ifAuAu200
  row.a[0]	 =  1.18231e-02;//
  row.a[1]	 = -1.23456e-04;//
  row.a[2]	 =  3.67595e-07;//
#endif
#endif
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =    2;
#if 0
  row.npar       =    4;           // Z3CGFHist848P04ifAuAu200
  row.min        =  20.;
  row.max        = 210.;
  row.a[0]	 =   4.23813e-02;//
  row.a[1]	 =   1.48021e-03;//
  row.a[2]	 =  -1.95910e-05;//
  row.a[3]	 =   5.25384e-08;//
#if 0
  row.nrows      = nrows;
  row.idx        =    2;
  row.npar       =    4;           // Z3GFHist841P04ifAuAu200
#if 0
  row.min        =  20.;
  row.max        = 210.;
#endif
  row.a[0]	 =   4.23991e-02;//
  row.a[1]	 =   1.40601e-03;//
  row.a[2]	 =  -1.87682e-05;//
  row.a[3]	 =   5.02144e-08;//
#endif
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
