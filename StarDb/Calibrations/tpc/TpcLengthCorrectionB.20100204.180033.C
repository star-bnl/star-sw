TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu200_production_ReversedFullField
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -2.41539e+01;//
  row.a[1]	 =  3.58922e+01;//
  row.a[2]	 = -1.96971e+01;//
  row.a[3]	 =  4.09447e+00;//
  row.a[4]	 =  3.01743e-01;//
  row.a[5]	 = -2.90594e-01;//
  row.a[6]	 =  4.85650e-02;//
  row.a[7]	 = -2.73877e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i_AuAu200_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.25579e-01; 
  row.a[1]	 =  1.05133e-01;
  row.a[2]	 = -5.00837e-02;
  row.a[3]	 =  5.33275e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu200_production_ReversedFullField
  row.npar       =            8;// fit mu
  row.a[0]	 = -2.27528e+01;//
  row.a[1]	 =  3.44318e+01;//
  row.a[2]	 = -1.92850e+01;//
  row.a[3]	 =  4.12228e+00;//
  row.a[4]	 =  2.80591e-01;//
  row.a[5]	 = -2.92739e-01;//
  row.a[6]	 =  4.99741e-02;//
  row.a[7]	 = -2.86670e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu200_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  8.26782e-02; 
  row.a[1]	 =  1.34187e-01;
  row.a[2]	 = -5.69776e-02;
  row.a[3]	 =  5.88857e-03;
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

