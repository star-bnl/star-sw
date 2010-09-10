TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu11_production_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 =  3.31149e+01;//
  row.a[1]	 = -5.89689e+01;//
  row.a[2]	 =  3.88837e+01;//
  row.a[3]	 = -1.01062e+01;//
  row.a[4]	 = -2.17973e-01;//
  row.a[5]	 =  5.32778e-01;//
  row.a[6]	 = -9.41923e-03;//
  row.a[7]	 = -3.35206e-02;//
  row.a[8]	 =  6.49949e-03;//
  row.a[9]	 = -3.83330e-04;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i_AuAu11_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  2.81113e-01; 
  row.a[1]	 = -4.81000e-02;
  row.a[2]	 = -4.09795e-03;
  row.a[3]	 =  9.49469e-04;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu11_production_ReversedFullField
  row.npar       =            9;// fit mu
  row.a[0]	 =  6.13861e+01;//
  row.a[1]	 = -1.16269e+02;//
  row.a[2]	 =  8.72357e+01;//
  row.a[3]	 = -3.08095e+01;//
  row.a[4]	 =  3.51776e+00;//
  row.a[5]	 =  1.01308e+00;//
  row.a[6]	 = -4.11417e-01;//
  row.a[7]	 =  5.52426e-02;//
  row.a[8]	 = -2.71786e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu11_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  1.56602e-01; 
  row.a[1]	 =  4.55913e-02;
  row.a[2]	 = -2.78084e-02;
  row.a[3]	 =  2.94292e-03;
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

