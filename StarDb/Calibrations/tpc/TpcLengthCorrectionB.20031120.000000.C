TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
#if 0
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist845P04ifAuAu200
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -3.48822e-01;//
  row.a[1]	 =  8.50199e-02;//
  row.a[2]	 = -7.84861e-03;//
  row.a[3]	 =  3.74962e-04;//
  row.a[4]	 = -1.05833e-05;//
  row.a[5]	 =  1.86272e-07;//
  row.a[6]	 = -2.06527e-09;//
  row.a[7]	 =  1.40134e-11;//
  row.a[8]	 = -5.31214e-14;//
  row.a[9]	 =  8.61426e-17;//
#endif
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPoints70BGPHist846P04ifAuAu200A
  row.a[0]	 =  4.41195e-01; 
  row.a[1]	 = -1.35098e-01;
  row.a[2]	 =  1.30491e-02;
  row.a[3]	 = -3.00213e-04;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
#if 0
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist845P04ifAuAu200
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -7.68754e-01;//
  row.a[1]	 =  1.59538e-01;//
  row.a[2]	 = -1.34980e-02;//
  row.a[3]	 =  6.11862e-04;//
  row.a[4]	 = -1.66460e-05;//
  row.a[5]	 =  2.84866e-07;//
  row.a[6]	 = -3.08864e-09;//
  row.a[7]	 =  2.05777e-11;//
  row.a[8]	 = -7.68203e-14;//
  row.a[9]	 =  1.22955e-16;//
#endif
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            6;//TPointsBGPHist845P04ifAuAu200
  row.a[0]	 =  5.55266e+00; 
  row.a[1]	 = -7.41100e+00;
  row.a[2]	 =  4.07830e+00;
  row.a[3]	 = -1.11718e+00;
  row.a[4]	 =  1.51133e-01;
  row.a[5]	 = -8.06903e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
#if 0
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 6 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 8 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 9 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 10 -> I70 extra correction for old clustering
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 11
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 12 -> I extra correction for old clustering
#endif
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
