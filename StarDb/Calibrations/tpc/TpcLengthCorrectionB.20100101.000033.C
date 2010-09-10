TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.min        =  TMath::Log(10);
  row.max        =  TMath::Log(150);
  row.a[0]	 = -1.23236e+01;//
  row.a[1]	 =  1.80715e+01;//
  row.a[2]	 = -9.76513e+00;//
  row.a[3]	 =  1.98411e+00;//
  row.a[4]	 =  1.53014e-01;//
  row.a[5]	 = -1.38346e-01;//
  row.a[6]	 =  2.24371e-02;//
  row.a[7]	 = -1.22559e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  2.21406e-01; 
  row.a[1]	 =  7.50204e-03;
  row.a[2]	 = -1.98191e-02;
  row.a[3]	 =  2.35566e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i
  row.npar       =            8;// fit mu
  row.min        =  TMath::Log(10);
  row.max        =  TMath::Log(160);
  row.a[0]	 = -8.50351e+00;//
  row.a[1]	 =  1.31956e+01;//
  row.a[2]	 = -7.56355e+00;//
  row.a[3]	 =  1.64979e+00;//
  row.a[4]	 =  1.12739e-01;//
  row.a[5]	 = -1.19350e-01;//
  row.a[6]	 =  2.04256e-02;//
  row.a[7]	 = -1.16752e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i
  row.npar       =            4;// fit sigma
  row.a[0]	 =   1.28317e-01; 
  row.a[1]	 =   7.64413e-02;
  row.a[2]	 =  -3.71342e-02;
  row.a[3]	 =   3.80599e-03;
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

