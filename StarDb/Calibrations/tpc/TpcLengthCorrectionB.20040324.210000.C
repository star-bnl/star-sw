TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist887P04ig_AuAu_62
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -3.69388e-01;//
  row.a[1]	 =  8.20048e-02;//
  row.a[2]	 = -7.02404e-03;//
  row.a[3]	 =  3.15545e-04;//
  row.a[4]	 = -8.40400e-06;//
  row.a[5]	 =  1.39886e-07;//
  row.a[6]	 = -1.47091e-09;//
  row.a[7]	 =  9.49634e-12;//
  row.a[8]	 = -3.43701e-14;//
  row.a[9]	 =  5.33953e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPointsBGPHist887P04ig_AuAu_62
  row.a[0]	 =  1.81621e-01; 
  row.a[1]	 =  4.56057e-02;
  row.a[2]	 = -2.95808e-02;
  row.a[3]	 =  3.09831e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist887P04ig_AuAu_62
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -4.19542e-01;//
  row.a[1]	 =  8.57841e-02;//
  row.a[2]	 = -7.12928e-03;//
  row.a[3]	 =  3.17400e-04;//
  row.a[4]	 = -8.46422e-06;//
  row.a[5]	 =  1.41794e-07;//
  row.a[6]	 = -1.50421e-09;//
  row.a[7]	 =  9.80746e-12;//
  row.a[8]	 = -3.58579e-14;//
  row.a[9]	 =  5.62754e-17;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            4;// TPointsBGPHist887P04ig_AuAu_62
  row.a[0]	 =  1.07634e-01; 
  row.a[1]	 =  1.02590e-01;
  row.a[2]	 = -4.48802e-02;
  row.a[3]	 =  4.49795e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
