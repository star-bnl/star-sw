TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; //TPoints70BUGPRunVII41dEdx3
  row.npar       =            -10;
  row.a[0]	 =   -1.03626e-01;//
  row.a[1]	 =    2.56486e-02;//
  row.a[2]	 =   -2.19982e-03;//
  row.a[3]	 =    9.35086e-05;//
  row.a[4]	 =   -2.30522e-06;//
  row.a[5]	 =    3.52651e-08;//
  row.a[6]	 =   -3.40186e-10;//
  row.a[7]	 =    2.01473e-12;//
  row.a[8]	 =   -6.69182e-15;//
  row.a[9]	 =    9.53375e-18;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            4;// TPoints70BUGPHist027P05ie_ittf
  row.min        =          2.3;//
  row.max        =          4.7;//                         
  row.a[0]	 = -7.48715e-02; 
  row.a[1]	 =  2.43322e-01;
  row.a[2]	 = -7.93629e-02;
  row.a[3]	 =  7.26962e-03;
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
  row.npar       =          -10;// TPointsBUGPHist027P05ie_ittf
  row.a[0]	 = -3.77307e-01;//
  row.a[1]	 =  7.36275e-02;//
  row.a[2]	 = -5.78515e-03;//
  row.a[3]	 =  2.40357e-04;//
  row.a[4]	 = -5.93463e-06;//
  row.a[5]	 =  9.15412e-08;//
  row.a[6]	 = -8.89930e-10;//
  row.a[7]	 =  5.29217e-12;//
  row.a[8]	 = -1.75597e-14;//
  row.a[9]	 =  2.48654e-17;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            5;// TPointsBUGPHist027P05ie_ittf
  row.min        =          2.3;//
  row.max        =          4.7;//                         
  row.a[0]	 =  2.18763e+00; 
  row.a[1]	 = -2.33189e+00;
  row.a[2]	 =  1.00102e+00;
  row.a[3]	 = -1.91316e-01;
  row.a[4]	 =  1.35191e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

