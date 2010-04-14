TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // /star/institutions/lbl/xueliang/offlinedEdxCali/workdir/dEdx/Histogram/pass2/TPoints70BUGPRunXP10if.root
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.min        =  TMath::Log(10);
  row.max        =  TMath::Log(130);
  row.a[0]	 = -1.73374e+01;//
  row.a[1]	 =  2.60196e+01;//
  row.a[2]	 = -1.43545e+01;//
  row.a[3]	 =  3.00217e+00;//
  row.a[4]	 =  2.15324e-01;//
  row.a[5]	 = -2.12357e-01;//
  row.a[6]	 =  3.57660e-02;//
  row.a[7]	 = -2.03457e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// /star/institutions/lbl/xueliang/offlinedEdxCali/workdir/dEdx/Histogram/pass2/TPoints70BUGPRunXP10if.root
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.90168e-01; 
  row.a[1]	 =  5.85766e-02;
  row.a[2]	 = -3.88727e-02;
  row.a[3]	 =  4.43885e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// /star/institutions/lbl/xueliang/offlinedEdxCali/workdir/dEdx/Histogram/pass2/TPointsBUGPRunXP10if.root
  row.npar       =            8;// fit mu
  row.min        =  TMath::Log(10);
  row.max        =  TMath::Log(130);
  row.a[0]	 = -2.58021e+01;//
  row.a[1]	 =  3.72415e+01;//
  row.a[2]	 = -1.98304e+01;//
  row.a[3]	 =  3.97042e+00;//
  row.a[4]	 =  3.18454e-01;//
  row.a[5]	 = -2.81449e-01;//
  row.a[6]	 =  4.61078e-02;//
  row.a[7]	 = -2.57135e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// /star/institutions/lbl/xueliang/offlinedEdxCali/workdir/dEdx/Histogram/pass2/TPointsBUGPRunXP10if.root
  row.npar       =            4;// fit sigma
  row.a[0]	 =  -2.29239e-02; 
  row.a[1]	 =   2.24084e-01;
  row.a[2]	 =  -8.20046e-02;
  row.a[3]	 =   8.18430e-03;
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

