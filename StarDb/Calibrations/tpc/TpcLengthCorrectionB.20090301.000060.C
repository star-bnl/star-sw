TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunIX58P09if_calib
  row.npar       =          -10;
  row.max        =  4.787491743;// log(120)
  row.a[0]	 =  1.32940e-01;//
  row.a[1]	 = -2.26992e-02;//
  row.a[2]	 =  2.41175e-03;//
  row.a[3]	 = -1.29859e-04;//
  row.a[4]	 =  3.99750e-06;//
  row.a[5]	 = -7.47514e-08;//
  row.a[6]	 =  8.62972e-10;//
  row.a[7]	 = -6.00016e-12;//
  row.a[8]	 =  2.30079e-14;//
  row.a[9]	 = -3.73491e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunIX58P09if_calib
  row.npar       =            6;// 
  row.max        =  4.787491743;// log(120)
  row.a[0]	 =  8.10648e+00; 
  row.a[1]	 = -1.09941e+01;
  row.a[2]	 =  6.02823e+00;
  row.a[3]	 = -1.63354e+00;
  row.a[4]	 =  2.17758e-01;
  row.a[5]	 = -1.14178e-02;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      =        nrows;
  row.idx        =            5;       // TPointsBUGPRunIX58P09if_calib
  row.npar       =            8;// 
  row.a[0]	 = -4.36264e+00;//
  row.a[1]	 =  4.20104e+00;//
  row.a[2]	 = -1.08481e+00;//
  row.a[3]	 = -8.18622e-02;//
  row.a[4]	 =  5.18650e-02;//
  row.a[5]	 =  5.57451e-03;//
  row.a[6]	 = -3.30309e-03;//
  row.a[7]	 =  2.88781e-04;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            6;// 
  row.a[0]	 =   2.75102e+00; 
  row.a[1]	 =  -3.79753e+00;
  row.a[2]	 =   2.20312e+00;
  row.a[3]	 =  -6.28044e-01;
  row.a[4]	 =   8.69443e-02;
  row.a[5]	 =  -4.67473e-03;
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

