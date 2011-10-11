TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUG
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -9.12671e+00;//
  row.a[1]	 =  1.11719e+01;//
  row.a[2]	 = -4.18035e+00;//
  row.a[3]	 =  8.51417e-02;//
  row.a[4]	 =  1.99019e-01;//
  row.a[5]	 =  5.74747e-03;//
  row.a[6]	 = -1.02944e-02;//
  row.a[7]	 = -1.05233e-03;//
  row.a[8]	 =  6.59857e-04;//
  row.a[9]	 = -5.62812e-05;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUG
  row.npar       =            6;// fit sigma 
  row.a[0]	 =  7.65064e-01; 
  row.a[1]	 = -4.69868e-01;
  row.a[2]	 =  9.39789e-02;
  row.a[3]	 =  1.13490e-02;
  row.a[4]	 = -6.61109e-03;
  row.a[5]	 =  6.44639e-04;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUG
  row.npar       =           10;// fit mu
  row.a[0]	 =  9.41009e+00;//
  row.a[1]	 = -9.04370e+00;//
  row.a[2]	 =  2.24704e+00;//
  row.a[3]	 =  2.15358e-01;//
  row.a[4]	 = -9.83817e-02;//
  row.a[5]	 = -1.61834e-02;//
  row.a[6]	 =  3.88464e-03;//
  row.a[7]	 =  1.02893e-03;//
  row.a[8]	 = -2.82482e-04;//
  row.a[9]	 =  1.74323e-05;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUG
  row.npar       =            6;// fit sigma
  row.a[0]	 =  3.28221e-01; 
  row.a[1]	 = -9.98932e-02;
  row.a[2]	 =  1.30516e-02;
  row.a[3]	 =  1.90347e-03;
  row.a[4]	 = -1.34984e-03;
  row.a[5]	 =  1.63985e-04;
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

