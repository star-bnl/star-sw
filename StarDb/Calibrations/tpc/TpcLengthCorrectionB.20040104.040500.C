TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist887P04ig_AuAu_1
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 =  -3.78068e-01;//
  row.a[1]	 =   8.86096e-02;//
  row.a[2]	 =  -8.01231e-03;//
  row.a[3]	 =   3.78903e-04;//
  row.a[4]	 =  -1.06232e-05;//
  row.a[5]	 =   1.86059e-07;//
  row.a[6]	 =  -2.05576e-09;//
  row.a[7]	 =   1.39192e-11;//
  row.a[8]	 =  -5.27187e-14;//
  row.a[9]	 =   8.55196e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPoints70BGPHist887P04ig_AuAu_1
  row.a[0]	 =  2.89625e-01; 
  row.a[1]	 = -2.70248e-02;
  row.a[2]	 = -1.29079e-02;
  row.a[3]	 =  1.80794e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist845P04ifAuAu200
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 =  -6.78554e-01;//
  row.a[1]	 =   1.40012e-01;//
  row.a[2]	 =  -1.18091e-02;//
  row.a[3]	 =   5.36174e-04;//
  row.a[4]	 =  -1.46590e-05;//
  row.a[5]	 =   2.52696e-07;//
  row.a[6]	 =  -2.76443e-09;//
  row.a[7]	 =   1.86057e-11;//
  row.a[8]	 =  -7.02343e-14;//
  row.a[9]	 =   1.13763e-16;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            4;//TPointsBGPHist887P04ig_AuAu_1
  row.a[0]	 =  8.49281e-02; 
  row.a[1]	 =  1.40315e-01;
  row.a[2]	 = -5.79786e-02;
  row.a[3]	 =  5.79792e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
