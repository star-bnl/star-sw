TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; //TPoints70BUGPRunII04P07id.root
  row.npar       =             10;
  row.a[0]	 =    5.82231e+01;//
  row.a[1]	 =   -9.64900e+01;//
  row.a[2]	 =    5.90073e+01;//
  row.a[3]	 =   -1.38436e+01;//
  row.a[4]	 =   -6.32623e-01;//
  row.a[5]	 =    7.41243e-01;//
  row.a[6]	 =   -5.73044e-03;//
  row.a[7]	 =   -4.22723e-02;//
  row.a[8]	 =    7.62536e-03;//
  row.a[9]	 =   -4.24018e-04;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            3;// TPoints70BUGPHist027P05ie_ittf
  row.min        =          2.3;//
  row.max        =          4.7;//                         
  row.a[0]	 =  6.21603e-01; 
  row.a[1]	 = -2.28409e-01;
  row.a[2]	 =  2.36501e-02;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     3;
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     4;
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;      
  row.npar       =            6;// TPointsBUGPHist027P05ie_ittf
  row.a[0]	 = -1.38519e+01;//
  row.a[1]	 =  1.93036e+01;//
  row.a[2]	 = -1.04948e+01;//
  row.a[3]	 =  2.77157e+00;//
  row.a[4]	 = -3.54907e-01;//
  row.a[5]	 =  1.76015e-02;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            5;// TPointsBUGPHist027P05ie_ittf
  row.min        =          2.3;//
  row.max        =          4.7;//                         
  row.a[0]	 =  2.57795e+00; 
  row.a[1]	 = -2.87101e+00;
  row.a[2]	 =  1.27049e+00;
  row.a[3]	 = -2.49089e-01;
  row.a[4]	 =  1.80042e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

