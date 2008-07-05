TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; // TPoints70BUGPRunVIII13P08ic_production_dAu2008_ReversedFullField
  row.min        =             24;
  row.max        =            140;
  row.npar       =            -10;
  row.a[0]	 =    3.19573e+00;//
  row.a[1]	 =   -4.35750e-01;//
  row.a[2]	 =    2.53734e-02;//
  row.a[3]	 =   -8.40045e-04;//
  row.a[4]	 =    1.74933e-05;//
  row.a[5]	 =   -2.37267e-07;//
  row.a[6]	 =    2.09173e-09;//
  row.a[7]	 =   -1.15436e-11;//
  row.a[8]	 =    3.61753e-14;//
  row.a[9]	 =   -4.90708e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            6;// TPoints70BGPRunVIII16P08ic_production_dAu2008_ReversedFullField
  row.a[0]	 =  8.72476e+00; 
  row.a[1]	 = -1.10759e+01;
  row.a[2]	 =  5.60081e+00;
  row.a[3]	 = -1.37969e+00;
  row.a[4]	 =  1.64177e-01;
  row.a[5]	 = -7.49300e-03;
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 1 -> sigma.I70
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      = nrows;
  row.idx        =     5;       // TPointsBUGPRunVIII13P08ic_production_dAu2008_ReversedFullField
  row.min        =    25;
  row.max        =   140;
  row.npar       =          -10;// 
  row.a[0]       =    4.54092e+00;//                            
  row.a[1]       =   -6.01599e-01;//                            
  row.a[2]       =    3.38973e-02;//                            
  row.a[3]       =   -1.08407e-03;//                            
  row.a[4]       =    2.18415e-05;//                            
  row.a[5]       =   -2.87900e-07;//                            
  row.a[6]       =    2.48118e-09;//                            
  row.a[7]       =   -1.34670e-11;//                            
  row.a[8]       =    4.17356e-14;//                            
  row.a[9]       =   -5.62474e-17;//                            
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            6;//  TPointsBGPRunVIII16P08ic_production_dAu2008_ReversedFullField
  row.a[0]	 =  4.46105e+00; 
  row.a[1]	 = -4.84698e+00;
  row.a[2]	 =  1.99859e+00;
  row.a[3]	 = -3.49933e-01;
  row.a[4]	 =  1.87123e-02;
  row.a[5]	 =  6.30289e-04;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;       // TPoints70BGPRunVIII14P08ic_production_dAu2008_ReversedFullField
  row.npar       =          -10;// 
  row.a[0]       =    1.27417e-01;//                            
  row.a[1]       =   -2.60300e-02;//                            
  row.a[2]       =    1.94985e-03;//                            
  row.a[3]       =   -7.61042e-05;//                            
  row.a[4]       =    1.76917e-06;//                            
  row.a[5]       =   -2.58884e-08;//                            
  row.a[6]       =    2.41036e-10;//                            
  row.a[7]       =   -1.38604e-12;//                            
  row.a[8]       =    4.48620e-15;//                            
  row.a[9]       =   -6.24764e-18;//                            
  tableSet->AddAt(&row);// 6 -> dI70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> dI60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     8;       // TPointsBGPRunVIII14P08ic_production_dAu2008_ReversedFullField
  row.npar       =          -9;// 
  row.a[0]       =    9.86854e-02;//                            
  row.a[1]       =   -2.03303e-02;//                            
  row.a[2]       =    1.39190e-03;//                            
  row.a[3]       =   -4.68213e-05;//                            
  row.a[4]       =    8.89744e-07;//                            
  row.a[5]       =   -1.00335e-08;//                            
  row.a[6]       =    6.65876e-11;//                            
  row.a[7]       =   -2.40360e-13;//                            
  row.a[8]       =    3.64012e-16;//                            
  tableSet->AddAt(&row);// 8 -> dIfit
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

