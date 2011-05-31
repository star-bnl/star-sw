TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunXI19AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -6.23534e+00;//
  row.a[1]	 =  7.49542e+00;//
  row.a[2]	 = -2.71606e+00;//
  row.a[3]	 =  4.37040e-02;//
  row.a[4]	 =  1.23720e-01;//
  row.a[5]	 =  4.10495e-03;//
  row.a[6]	 = -6.09094e-03;//
  row.a[7]	 = -6.38239e-04;//
  row.a[8]	 =  3.75120e-04;//
  row.a[9]	 = -3.09338e-05;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunXI19AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.82795e-01; 
  row.a[1]	 =  4.79756e-02;
  row.a[2]	 = -3.30277e-02;
  row.a[3]	 =  3.72101e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunXI19AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10;// fit mu
  row.a[0]	 = -3.06025e+00;//
  row.a[1]	 =  3.93570e+00;//
  row.a[2]	 = -1.52557e+00;//
  row.a[3]	 =  3.92708e-02;//
  row.a[4]	 =  7.22500e-02;//
  row.a[5]	 =  2.10073e-03;//
  row.a[6]	 = -3.78026e-03;//
  row.a[7]	 = -3.81140e-04;//
  row.a[8]	 =  2.37292e-04;//
  row.a[9]	 = -1.99322e-05;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunXI19AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  1.11175e-01; 
  row.a[1]	 =  9.79616e-02;
  row.a[2]	 = -4.49989e-02;
  row.a[3]	 =  4.68559e-03;
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

