TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; //TPoints70BUGPHist027P05ie_ittf
  row.npar       =             -9;
  row.a[0]	 =   -9.66152e-02;//
  row.a[1]	 =    1.95722e-02;//
  row.a[2]	 =   -1.46613e-03;//
  row.a[3]	 =    5.44171e-05;//
  row.a[4]	 =   -1.10927e-06;//
  row.a[5]	 =    1.30748e-08;//
  row.a[6]	 =   -8.88083e-11;//
  row.a[7]	 =    3.22147e-13;//
  row.a[8]	 =   -4.82477e-16;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            4;// TPoints70BUGPHist027P05ie_ittf
  row.min        =          2.3;//
  row.max        =          4.7;//                         
  row.a[0]	 = -3.14706e-01; 
  row.a[1]	 =  4.04681e-01;
  row.a[2]	 = -1.16752e-01;
  row.a[3]	 =  1.01701e-02;
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
  row.a[0]	 = -6.24201e-03;//
  row.a[1]	 =  1.30656e-02;//
  row.a[2]	 = -1.69159e-03;//
  row.a[3]	 =  9.15113e-05;//
  row.a[4]	 = -2.65767e-06;//
  row.a[5]	 =  4.60824e-08;//
  row.a[6]	 = -4.94144e-10;//
  row.a[7]	 =  3.22124e-12;//
  row.a[8]	 = -1.17160e-14;//
  row.a[9]	 =  1.82422e-17;//
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            4;// TPointsBUGPHist027P05ie_ittf
  row.a[0]	 = -5.33286e-01; 
  row.a[1]	 =  5.80867e-01;
  row.a[2]	 = -1.64059e-01;
  row.a[3]	 =  1.43814e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

