TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunXI15dev_calib_pp500_production_2011_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -8.83938e+00;//
  row.a[1]	 =  1.06149e+01;//
  row.a[2]	 = -3.86761e+00;//
  row.a[3]	 =  7.32703e-02;//
  row.a[4]	 =  1.75897e-01;//
  row.a[5]	 =  5.38613e-03;//
  row.a[6]	 = -8.82679e-03;//
  row.a[7]	 = -9.26412e-04;//
  row.a[8]	 =  5.61963e-04;//
  row.a[9]	 = -4.74498e-05;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunXI15dev_calib_pp500_production_2011_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  8.85726e-02; 
  row.a[1]	 =  1.31213e-01;
  row.a[2]	 = -5.68628e-02;
  row.a[3]	 =  5.93559e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBGPRunXI15dev_calib_pp500_production_2011_ReversedFullField
  row.npar       =           10;// fit mu
  row.a[0]	 = -3.42966e+00;//
  row.a[1]	 =  4.51345e+00;//
  row.a[2]	 = -1.80353e+00;//
  row.a[3]	 =  5.50259e-02;//
  row.a[4]	 =  8.89026e-02;//
  row.a[5]	 =  2.07187e-03;//
  row.a[6]	 = -4.84113e-03;//
  row.a[7]	 = -5.17038e-04;//
  row.a[8]	 =  3.31952e-04;//
  row.a[9]	 = -2.89716e-05;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu11_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  1.93295e-03; 
  row.a[1]	 =  1.93847e-01;
  row.a[2]	 = -7.21356e-02;
  row.a[3]	 =  7.17507e-03;
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

