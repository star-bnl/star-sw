TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =       nrows;
  row.idx        =           1;
  row.npar       =          -2;//Track lenths instead of log Points70BUGPHist963P05ia_dedx
  row.min        =           0.;//  default clustering == fcf                     
  row.max        =           0.;//                         
  row.a[0]	 = -2.32495e-02;//
  row.a[1]	 =  3.58730e-04;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            3;//Points70BUGPHist963P05ia_dedx
  row.a[0]	 =  3.99909e-01; 
  row.a[1]	 = -1.13657e-01;
  row.a[2]	 =  8.97040e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =       nrows;
  row.idx        =           5;
  row.npar       =          -2;//Track lenths instead of log TPointsBUGPHist964P05ia_dedx
  row.min        =           0.;//  default clustering == fcf                     
  row.max        =           0.;//                         
  row.a[0]	 = 4.78470e-02;//
  row.a[1]	 = 4.73639e-04;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            6;
  row.npar       =            4;//TPointsBUGPHist964P05ia_dedx
  row.a[0]	 =  7.20752e-03; 
  row.a[1]	 =  1.95111e-01;
  row.a[2]	 = -7.14778e-02;
  row.a[3]	 =  6.93481e-03;
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
