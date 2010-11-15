TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu200_production_FullField
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -1.05560e+01;//
  row.a[1]	 =  1.52593e+01;//
  row.a[2]	 = -8.11208e+00;//
  row.a[3]	 =  1.60715e+00;//
  row.a[4]	 =  1.39579e-01;//
  row.a[5]	 = -1.16467e-01;//
  row.a[6]	 =  1.87502e-02;//
  row.a[7]	 = -1.02418e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunX36P10ij_dedx_AuAu200_production_FullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.86826e-01; 
  row.a[1]	 =  4.58102e-02;
  row.a[2]	 = -3.18665e-02;
  row.a[3]	 =  3.52480e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu200_production_FullField
  row.npar       =            8;// fit mu
  row.a[0]	 =  -8.59749e+00;//
  row.a[1]	 =   1.32404e+01;//
  row.a[2]	 =  -7.55007e+00;//
  row.a[3]	 =   1.64812e+00;//
  row.a[4]	 =   1.10349e-01;//
  row.a[5]	 =  -1.19601e-01;//
  row.a[6]	 =   2.07370e-02;//
  row.a[7]	 =  -1.20398e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX36P10ij_dedx_AuAu200_production_FullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =   1.32296e-01; 
  row.a[1]	 =   8.45536e-02;
  row.a[2]	 =  -4.13806e-02;
  row.a[3]	 =   4.31134e-03;
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

