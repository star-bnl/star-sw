TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist887P04ig_AuAu_3
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -3.98397e-01;//
  row.a[1]	 =  1.07612e-01;//
  row.a[2]	 = -1.07696e-02;//
  row.a[3]	 =  5.50683e-04;//
  row.a[4]	 = -1.64732e-05;//
  row.a[5]	 =  3.05254e-07;//
  row.a[6]	 = -3.55013e-09;//
  row.a[7]	 =  2.52269e-11;//
  row.a[8]	 = -1.00114e-13;//
  row.a[9]	 =  1.70021e-16;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPoints70BGPHist887P04ig_AuAu_3
  row.a[0]	 =  4.29543e-01; 
  row.a[1]	 = -1.08996e-01;
  row.a[2]	 =  2.32557e-03;
  row.a[3]	 =  9.63570e-04;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist887P04ig_AuAu_3
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -1.08608e+00;//
  row.a[1]	 =  2.36204e-01;//
  row.a[2]	 = -2.09718e-02;//
  row.a[3]	 =  9.99094e-04;//
  row.a[4]	 = -2.85580e-05;//
  row.a[5]	 =  5.13286e-07;//
  row.a[6]	 = -5.84655e-09;//
  row.a[7]	 =  4.09549e-11;//
  row.a[8]	 = -1.60939e-13;//
  row.a[9]	 =  2.71498e-16;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            4;// TPointsBGPHist887P04ig_AuAu_3
  row.a[0]	 =  1.28002e-01; 
  row.a[1]	 =  1.30734e-01;
  row.a[2]	 = -6.05583e-02;
  row.a[3]	 =  6.39445e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
