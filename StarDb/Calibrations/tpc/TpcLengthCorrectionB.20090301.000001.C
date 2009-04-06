TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; // TPoints70BUGPRunIX02dev_lowLuminosity2009_ReversedFullField
  row.npar       =            -10;
  row.min        =             10;
  row.max        =            150;
  row.a[0]	 =   -8.97416e-02;//
  row.a[1]	 =    1.51459e-02;//
  row.a[2]	 =    9.34931e-05;//
  row.a[3]	 =   -6.23093e-05;//
  row.a[4]	 =    2.98964e-06;//
  row.a[5]	 =   -6.78907e-08;//
  row.a[6]	 =    8.62070e-10;//
  row.a[7]	 =   -6.26086e-12;//
  row.a[8]	 =    2.43157e-14;//
  row.a[9]	 =   -3.91886e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunIX02dev_lowLuminosity2009_ReversedFullField
  row.npar       =           -5;// 
  row.a[0]	 =   2.25801e-01; 
  row.a[1]	 =  -4.92784e-03;
  row.a[2]	 =   6.41809e-05;
  row.a[3]	 =  -3.96156e-07;
  row.a[4]	 =   9.49243e-10;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      = nrows;
  row.idx        =     5;       // TPointsBUGPRunIX02dev_lowLuminosity2009_ReversedFullField
  row.npar       =           -8;// 
  row.min        =           24;
  row.max        =          150;
  row.a[0]	 =  2.27834e+00;//
  row.a[1]	 = -2.34474e-01;//
  row.a[2]	 =  9.73010e-03;//
  row.a[3]	 = -2.11236e-04;//
  row.a[4]	 =  2.61021e-06;//
  row.a[5]	 = -1.84683e-08;//
  row.a[6]	 =  6.97034e-11;//
  row.a[7]	 = -1.08861e-13;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =           -4;// 
  row.a[0]	 =   2.30161e-01; 
  row.a[1]	 =  -4.14354e-03;
  row.a[2]	 =   3.64565e-05;
  row.a[3]	 =  -1.08885e-07;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 6 -> dI70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> dI60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 8 -> dIfit
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

