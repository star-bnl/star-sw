TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BGPRunXI37AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10; // npar > 0 means you fit with log(X), fit mu
  row.a[0]	 = -1.01373e+01+6.84180e-01;//
  row.a[1]	 =  1.18828e+01-5.60394e-01;//
  row.a[2]	 = -4.20417e+00+8.60232e-02;//
  row.a[3]	 =  5.29135e-02+2.38846e-02;//
  row.a[4]	 =  1.85739e-01-1.25858e-03;//
  row.a[5]	 =  7.16810e-03-1.35180e-03;//
  row.a[6]	 = -8.77785e-03-1.85494e-04;//
  row.a[7]	 = -9.74647e-04+3.92523e-05;//
  row.a[8]	 =  5.27105e-04+1.77242e-05;//
  row.a[9]	 = -4.19065e-05-2.61048e-06;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunXI39AuAu19P11id_dedx
  row.npar       =            6;// fit sigma 
  row.a[0]	 =  4.68239e+00+4.61829e+00; 
  row.a[1]	 = -6.26132e+00-6.15309e+00;
  row.a[2]	 =  3.43289e+00+3.36440e+00;
  row.a[3]	 = -9.31805e-01-9.11225e-01;
  row.a[4]	 =  1.24312e-01+1.21338e-01;
  row.a[5]	 = -6.51452e-03-6.34726e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBGPRunXI37AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
  row.npar       =           10;// fit mu
  row.a[0]	 = -9.81532e+00+5.80712e-01;//
  row.a[1]	 =  1.16685e+01-4.90055e-01;//
  row.a[2]	 = -4.18542e+00+7.97081e-02;//
  row.a[3]	 =  6.08861e-02+2.07491e-02;//
  row.a[4]	 =  1.85969e-01-1.34451e-03;//
  row.a[5]	 =  6.89018e-03-1.21044e-03;//
  row.a[6]	 = -8.83331e-03-1.55620e-04;//
  row.a[7]	 = -9.71108e-04+3.63990e-05;//
  row.a[8]	 =  5.30297e-04+1.54038e-05;//
  row.a[9]	 = -4.22431e-05-2.31586e-06;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBGPRunXI39AuAu19P11id_dedx.root
  row.npar       =            6;// fit sigma
  row.a[0]	 =  4.46037e+00-3.85938e-02; 
  row.a[1]	 = -6.08921e+00+1.82130e-01;
  row.a[2]	 =  3.39901e+00-4.17470e-02;
  row.a[3]	 = -9.36023e-01-6.37609e-03;
  row.a[4]	 =  1.26329e-01+2.49601e-03;
  row.a[5]	 = -6.68271e-03-1.69537e-04;
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

