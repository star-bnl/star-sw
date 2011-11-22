TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BGPRunXI37AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -1.01373e+01+4.89760e-01;//
  row.a[1]	 =  1.18828e+01-3.76878e-01;//
  row.a[2]	 = -4.20417e+00+5.07638e-02;//
  row.a[3]	 =  5.29135e-02+1.58776e-02;//
  row.a[4]	 =  1.85739e-01-4.48834e-04;//
  row.a[5]	 =  7.16810e-03-8.16444e-04;//
  row.a[6]	 = -8.77785e-03-1.25712e-04;//
  row.a[7]	 = -9.74647e-04+2.08317e-05;//
  row.a[8]	 =  5.27105e-04+1.07301e-05;//
  row.a[9]	 = -4.19065e-05-1.47559e-06;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunXI39AuAu19P11id_dedx
  row.npar       =            6;// fit sigma 
  row.a[0]	 = +4.85809e+00; 
  row.a[1]	 = -6.55570e+00;
  row.a[2]	 = +3.61939e+00;
  row.a[3]	 = -9.88542e-01;
  row.a[4]	 = +1.32674e-01;
  row.a[5]	 = -6.99470e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBGPRunXI37AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10;// fit mu
  row.a[0]	 = -9.81532e+00+1.94066e+00;//
  row.a[1]	 =  1.16685e+01-2.09701e+00;//
  row.a[2]	 = -4.18542e+00+6.60400e-01;//
  row.a[3]	 =  6.08861e-02+1.07469e-02;//
  row.a[4]	 =  1.85969e-01-2.85080e-02;//
  row.a[5]	 =  6.89018e-03-1.95773e-03;//
  row.a[6]	 = -8.83331e-03+1.24894e-03;//
  row.a[7]	 = -9.71108e-04+1.72572e-04;//
  row.a[8]	 =  5.30297e-04-7.47355e-05;//
  row.a[9]	 = -4.22431e-05+5.54177e-06;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBGPRunXI39AuAu19P11id_dedx.root
  row.npar       =            6;// fit sigma
  row.a[0]	 = +4.15231e+00; 
  row.a[1]	 = -5.70374e+00;
  row.a[2]	 = +3.21107e+00;
  row.a[3]	 = -8.91563e-01;
  row.a[4]	 = +1.21250e-01;
  row.a[5]	 = -6.46000e-03;
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

