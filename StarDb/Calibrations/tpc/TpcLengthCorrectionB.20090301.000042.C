TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; // TPoints70BUGPRunIX42P09if
  row.npar       =             10;
  row.min        = 2.56494935746153674; // log(13)
  row.max        = 4.86753445045558220; // log(130)
  row.a[0]	 =   -3.01568e+01 -5.61142e-03;//
  row.a[1]	 =    3.35522e+01;//
  row.a[2]	 =   -1.10203e+01;//
  row.a[3]	 =   -8.97717e-02;//
  row.a[4]	 =    4.97372e-01;//
  row.a[5]	 =    3.07460e-02;//
  row.a[6]	 =   -2.37479e-02;//
  row.a[7]	 =   -3.20607e-03;//
  row.a[8]	 =    1.54293e-03;//
  row.a[9]	 =   -1.22348e-04;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// Points70BUGPRunIX41P09ifB
  row.npar       =            6;// 
  row.a[0]	 =   7.85003e+00; 
  row.a[1]	 =  -1.10231e+01;
  row.a[2]	 =   6.28449e+00;
  row.a[3]	 =  -1.77299e+00;
  row.a[4]	 =   2.46112e-01;
  row.a[5]	 =  -1.34350e-02;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      = nrows;
  row.idx        =     5;       // TPoints70BGPRunIX42P09if
  row.npar       =           10;// 
  row.min        = 2.63905732961525841; // log(14)
  row.max        = 4.82831373730230151; // log(125)
  row.a[0]	 = -5.35479e+01-4.90822e-03;//
  row.a[1]	 =  5.76434e+01;//
  row.a[2]	 = -1.79448e+01;//
  row.a[3]	 = -4.74060e-01;//
  row.a[4]	 =  8.26840e-01;//
  row.a[5]	 =  6.81727e-02;//
  row.a[6]	 = -3.88382e-02;//
  row.a[7]	 = -6.22664e-03;//
  row.a[8]	 =  2.64185e-03;//
  row.a[9]	 = -2.02383e-04;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            5;// 
  row.a[0]	 =   6.56503e+00; 
  row.a[1]	 =  -9.29393e+00;
  row.a[2]	 =   5.33788e+00;
  row.a[3]	 =  -1.51137e+00;
  row.a[4]	 =   2.09835e-01;
  row.a[5]	 =  -1.14264e-02;
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

