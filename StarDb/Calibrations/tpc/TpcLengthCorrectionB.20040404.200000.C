TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist887P04ig_pp
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -1.68971e-01;//
  row.a[1]	 =  3.48461e-02;//
  row.a[2]	 = -2.73306e-03;//
  row.a[3]	 =  1.10021e-04;//
  row.a[4]	 = -2.59084e-06;//
  row.a[5]	 =  3.84895e-08;//
  row.a[6]	 = -3.71858e-10;//
  row.a[7]	 =  2.30140e-12;//
  row.a[8]	 = -8.37638e-15;//
  row.a[9]	 =  1.36935e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            4;//TPoints70BGPHist887P04ig_pp
  row.a[0]	 = -2.04291e-01; 
  row.a[1]	 =  3.47395e-01;
  row.a[2]	 = -1.06794e-01;
  row.a[3]	 =  9.57019e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist887P04ig_pp
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         125.;//                         
  row.a[0]	 = -4.83899e-01;//
  row.a[1]	 =  1.05507e-01;//
  row.a[2]	 = -9.23142e-03;//
  row.a[3]	 =  4.25537e-04;//
  row.a[4]	 = -1.16611e-05;//
  row.a[5]	 =  2.01128e-07;//
  row.a[6]	 = -2.21172e-09;//
  row.a[7]	 =  1.50692e-11;//
  row.a[8]	 = -5.80077e-14;//
  row.a[9]	 =  9.64388e-17;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            4;// TPointsBGPHist887P04ig_pp
  row.a[0]	 = -5.52993e-01; 
  row.a[1]	 =  6.39271e-01;
  row.a[2]	 = -1.85861e-01;
  row.a[3]	 =  1.65590e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
