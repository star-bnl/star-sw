TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunX33P10i_AuAu39_production_ReversedFullField
  row.npar       =            8; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -1.15807e+01;//
  row.a[1]	 =  1.68410e+01;//
  row.a[2]	 = -8.99959e+00;//
  row.a[3]	 =  1.76312e+00;//
  row.a[4]	 =  1.79711e-01;//
  row.a[5]	 = -1.38229e-01;//
  row.a[6]	 =  2.21676e-02;//
  row.a[7]	 = -1.21198e-03;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunX33P10i_AuAu39_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.65657e-01; 
  row.a[1]	 =  5.35873e-02;
  row.a[2]	 = -3.26886e-02;
  row.a[3]	 =  3.55279e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX33P10i_AuAu39_production_ReversedFullField
  row.npar       =            8;// fit mu
  row.a[0]	 = -6.75653e+00;//
  row.a[1]	 =  1.05491e+01;//
  row.a[2]	 = -6.05094e+00;//
  row.a[3]	 =  1.27419e+00;//
  row.a[4]	 =  1.32228e-01;//
  row.a[5]	 = -1.10204e-01;//
  row.a[6]	 =  1.86471e-02;//
  row.a[7]	 = -1.06955e-03;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunX33P10i_AuAu39_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  5.37958e-02; 
  row.a[1]	 =  1.37664e-01;
  row.a[2]	 = -5.40120e-02;
  row.a[3]	 =  5.35304e-03;
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

