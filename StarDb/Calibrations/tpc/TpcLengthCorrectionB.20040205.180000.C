TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist887P04ig_AuAu_2
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 =   -2.95657e-01;//
  row.a[1]	 =    8.69359e-02;//
  row.a[2]	 =   -9.08473e-03;//
  row.a[3]	 =    4.78225e-04;//
  row.a[4]	 =   -1.46333e-05;//
  row.a[5]	 =    2.76408e-07;//
  row.a[6]	 =   -3.27040e-09;//
  row.a[7]	 =    2.36170e-11;//
  row.a[8]	 =   -9.52024e-14;//
  row.a[9]	 =    1.64218e-16;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPoints70BGPHist887P04ig_AuAu_2 
  row.a[0]	 =  5.56090e-01; 
  row.a[1]	 = -2.23346e-01;
  row.a[2]	 =  3.53238e-02;
  row.a[3]	 = -2.11852e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist887P04ig_AuAu_2
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 =   -9.90366e-01;//
  row.a[1]	 =    2.14384e-01;//
  row.a[2]	 =   -1.89596e-02;//
  row.a[3]	 =    9.01491e-04;//
  row.a[4]	 =   -2.57795e-05;//
  row.a[5]	 =    4.64639e-07;//
  row.a[6]	 =   -5.31884e-09;//
  row.a[7]	 =    3.75202e-11;//
  row.a[8]	 =   -1.48755e-13;//
  row.a[9]	 =    2.53613e-16;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            4;// TPointsBGPHist887P04ig_AuAu_2
  row.a[0]	 =  3.09848e-01; 
  row.a[1]	 = -2.95085e-02;
  row.a[2]	 = -1.54098e-02;
  row.a[3]	 =  2.28178e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
