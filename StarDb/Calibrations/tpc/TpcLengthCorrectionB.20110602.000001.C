TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BGPRunXI64AuAu200FF
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -9.52292e+00;//
  row.a[1]	 =  1.13211e+01;//
  row.a[2]	 = -4.03805e+00;//
  row.a[3]	 =  2.28992e-02;//
  row.a[4]	 =  1.95935e-01;//
  row.a[5]	 =  8.82709e-03;//
  row.a[6]	 = -1.01549e-02;//
  row.a[7]	 = -1.19887e-03;//
  row.a[8]	 =  6.78034e-04;//
  row.a[9]	 = -5.71198e-05;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunXI64AuAu200FF
  row.npar       =            6;// fit sigma 
  row.a[0]	 =  6.89537e-01; 
  row.a[1]	 = -4.21456e-01;
  row.a[2]	 =  9.01352e-02;
  row.a[3]	 =  7.10085e-03;
  row.a[4]	 = -5.37834e-03;
  row.a[5]	 =  5.45741e-04;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBGPRunXI64AuAu200FF
  row.npar       =           10;// fit mu
  row.a[0]	 = -6.29466e-01;//
  row.a[1]	 =  1.63776e+00;//
  row.a[2]	 = -9.83559e-01;//
  row.a[3]	 =  9.62160e-02;//
  row.a[4]	 =  5.36601e-02;//
  row.a[5]	 = -2.25851e-03;//
  row.a[6]	 = -3.38252e-03;//
  row.a[7]	 = -1.75956e-04;//
  row.a[8]	 =  2.25355e-04;//
  row.a[9]	 = -2.19632e-05;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBGPRunXI64AuAu200FF
  row.npar       =            6;// fit sigma
  row.a[0]	 =  4.20554e-01; 
  row.a[1]	 = -1.91357e-01;
  row.a[2]	 =  3.46366e-02;
  row.a[3]	 =  4.35932e-03;
  row.a[4]	 = -2.77812e-03;
  row.a[5]	 =  2.96264e-04;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 6 -> dI70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> dI60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 8 -> dIfit
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

