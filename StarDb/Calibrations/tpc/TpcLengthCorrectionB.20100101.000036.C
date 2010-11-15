TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX36P10ij
  row.npar       =           -8; // npar > 0 means you fit with log(X), fit mu
  row.min        =  TMath::Log(15);
  row.max        =  TMath::Log(120);
  row.a[0]	 =  1.70622e-01;//
  row.a[1]	 = -1.90749e-02;//
  row.a[2]	 =  1.05962e-03;//
  row.a[3]	 = -3.02542e-05;//
  row.a[4]	 =  4.84289e-07;//
  row.a[5]	 = -4.39765e-09;//
  row.a[6]	 =  2.12102e-11;//
  row.a[7]	 = -4.22810e-14;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunX36P10ij
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.39647e-01; 
  row.a[1]	 =  8.57889e-02;
  row.a[2]	 = -4.29780e-02;
  row.a[3]	 =  4.54159e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX36P10ij
  row.npar       =           -9;// fit mu
  row.min        =  TMath::Log(10);
  row.max        =  TMath::Log(160);
  row.a[0]	 = -2.64321e-03;//
  row.a[1]	 =  3.07064e-03;//
  row.a[2]	 = -1.18781e-04;//
  row.a[3]	 =  3.13686e-06;//
  row.a[4]	 = -6.38097e-08;//
  row.a[5]	 =  8.81389e-10;//
  row.a[6]	 = -7.18973e-12;//
  row.a[7]	 =  3.06487e-14;//
  row.a[8]	 = -5.24247e-17;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBGPRunX36P10ij
  row.npar       =            4;// fit sigma
  row.a[0]	 =   7.87811e-02; 
  row.a[1]	 =   1.29524e-01;
  row.a[2]	 =  -5.38343e-02;
  row.a[3]	 =   5.45004e-03;
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

