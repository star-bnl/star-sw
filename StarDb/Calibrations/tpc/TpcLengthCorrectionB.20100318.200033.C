TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu62_production_ReversedFullField
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -1.34921e+01;//
  row.a[1]	 =  1.96720e+01;//
  row.a[2]	 = -1.05282e+01;//
  row.a[3]	 =  2.07088e+00;//
  row.a[4]	 =  2.05307e-01;//
  row.a[5]	 = -1.60523e-01;//
  row.a[6]	 =  2.58229e-02;//
  row.a[7]	 = -1.41607e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i_AuAu62_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.82580e-01; 
  row.a[1]	 =  4.58078e-02;
  row.a[2]	 = -3.16083e-02;
  row.a[3]	 =  3.51692e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu62_production_ReversedFullField
  row.npar       =            8;// fit mu
  row.a[0]	 = -1.00137e+01;//
  row.a[1]	 =  1.52381e+01;//
  row.a[2]	 = -8.54373e+00;//
  row.a[3]	 =  1.78827e+00;//
  row.a[4]	 =  1.57348e-01;//
  row.a[5]	 = -1.40186e-01;//
  row.a[6]	 =  2.35826e-02;//
  row.a[7]	 = -1.34166e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu62_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  8.90378e-02; 
  row.a[1]	 =  1.16353e-01;
  row.a[2]	 = -4.96858e-02;
  row.a[3]	 =  5.06490e-03;
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

