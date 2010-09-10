TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu7_production_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 =  3.96318e+01;//
  row.a[1]	 = -6.64127e+01;//
  row.a[2]	 =  4.13493e+01;//
  row.a[3]	 = -1.00509e+01;//
  row.a[4]	 = -3.40619e-01;//
  row.a[5]	 =  5.21099e-01;//
  row.a[6]	 = -1.99684e-03;//
  row.a[7]	 = -3.28001e-02;//
  row.a[8]	 =  6.07431e-03;//
  row.a[9]	 = -3.47922e-04;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i_AuAu7_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  2.18624e-01; 
  row.a[1]	 =  4.45575e-03;
  row.a[2]	 = -1.85353e-02;
  row.a[3]	 =  2.24865e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu7_production_ReversedFullField
  row.npar       =            9;// fit mu
  row.a[0]	 =  7.21205e+01;//
  row.a[1]	 = -1.30183e+02;//
  row.a[2]	 =  9.32761e+01;//
  row.a[3]	 = -3.14091e+01;//
  row.a[4]	 =  3.28428e+00;//
  row.a[5]	 =  1.04926e+00;//
  row.a[6]	 = -3.99342e-01;//
  row.a[7]	 =  5.18668e-02;//
  row.a[8]	 = -2.48559e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu7_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  1.19080e-01; 
  row.a[1]	 =  7.70114e-02;
  row.a[2]	 = -3.64712e-02;
  row.a[3]	 =  3.72981e-03;
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

