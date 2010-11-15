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
  row.idx        =            2;// TPoints70BGPRunX36P10ij_dedx_AuAu200_production_ReversedFullField
  row.npar       =            4;// fit sigma 
  row.a[0]	 =  1.64972e-01; 
  row.a[1]	 =  6.84938e-02;
  row.a[2]	 = -3.88728e-02;
  row.a[3]	 =  4.21012e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunX36P10ij_dedx_AuAu200_production_ReversedFullField
  row.npar       =           -8;// fit mu
  row.a[0]	 =  2.11770e-02;//
  row.a[1]	 =  3.13171e-04;//
  row.a[2]	 =  4.00804e-05;//
  row.a[3]	 = -2.10101e-06;//
  row.a[4]	 =  3.89056e-08;//
  row.a[5]	 = -3.33766e-10;//
  row.a[6]	 =  1.32669e-12;//
  row.a[7]	 = -1.90885e-15;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBGPRunX36P10ij_dedx_AuAu200_production_ReversedFullField
  row.npar       =            4;// fit sigma
  row.a[0]	 =  1.33609e-01; 
  row.a[1]	 =  8.76923e-02;
  row.a[2]	 = -4.30465e-02;
  row.a[3]	 =  4.52081e-03;
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

