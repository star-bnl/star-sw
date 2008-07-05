TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =          nrows; // Correction/sigma versus LogTrackLength
  row.idx        =              1; // TPoints70BUGPRunVIII13P08ic_production_dAu2008_ReversedFullField
  row.npar       =            -10;
  row.min        =             24;
  row.max        =            150;
  row.a[0]	 =    3.26173e+00;//
  row.a[1]	 =   -4.42615e-01;//
  row.a[2]	 =    2.55910e-02;//
  row.a[3]	 =   -8.39168e-04;//
  row.a[4]	 =    1.72705e-05;//
  row.a[5]	 =   -2.31093e-07;//
  row.a[6]	 =    2.00700e-09;//
  row.a[7]	 =   -1.08977e-11;//
  row.a[8]	 =    3.35613e-14;//
  row.a[9]	 =   -4.46855e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;
  row.npar       =            6;// TPoints70BGPRunVIII16P08ic_ppProduction2008_ReversedFullField
  row.a[0]	 =   1.07761e+01; 
  row.a[1]	 =  -1.41270e+01;
  row.a[2]	 =   7.35909e+00;
  row.a[3]	 =  -1.87259e+00;
  row.a[4]	 =   2.31722e-01;
  row.a[5]	 =  -1.11285e-02;
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 1 -> sigma.I70
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      = nrows;
  row.idx        =     5;       // TPointsBUGPRunVIII13P08ic_ppProduction2008_ReversedFullField
  row.npar       =          -10;// 
  row.min        =             24;
  row.max        =            150;
  row.a[0]	 =  2.86866e+00;//
  row.a[1]	 = -3.74401e-01;//
  row.a[2]	 =  2.04385e-02;//
  row.a[3]	 = -6.28422e-04;//
  row.a[4]	 =  1.21285e-05;//
  row.a[5]	 = -1.52748e-07;//
  row.a[6]	 =  1.25477e-09;//
  row.a[7]	 = -6.47627e-12;//
  row.a[8]	 =  1.90399e-14;//
  row.a[9]	 = -2.42817e-17;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;
  row.npar       =            6;// TPointsBGPRunVIII16P08ic_ppProduction2008_ReversedFullField
  row.a[0]	 =   6.24374e+00; 
  row.a[1]	 =  -7.41528e+00;
  row.a[2]	 =   3.45832e+00;
  row.a[3]	 =  -7.59149e-01;
  row.a[4]	 =   7.53728e-02;
  row.a[5]	 =  -2.47356e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;       // TPoints70BGPRunVIII14P08ic_ppProduction2008_ReversedFullField
  row.npar       =          -10;// 
  row.a[0]       =    2.23486e-01;//                            
  row.a[1]       =   -3.98454e-02;//                            
  row.a[2]       =    2.59175e-03;//                            
  row.a[3]       =   -8.78407e-05;//                            
  row.a[4]       =    1.75917e-06;//                            
  row.a[5]       =   -2.18320e-08;//                            
  row.a[6]       =    1.68585e-10;//                            
  row.a[7]       =   -7.80831e-13;//                            
  row.a[8]       =    1.95742e-15;//                            
  row.a[9]       =   -1.99214e-18;//                            
  tableSet->AddAt(&row);// 6 -> dI70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> dI60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     8;       // TPointsBGPRunVIII14P08ic_ppProduction2008_ReversedFullField
  row.npar       =          -9;// 
  row.a[0]       =    1.48874e-02;//                            
  row.a[1]       =   -4.64604e-03;//                            
  row.a[2]       =    3.11196e-04;//                            
  row.a[3]       =   -9.08707e-06;//                            
  row.a[4]       =    1.38749e-07;//                            
  row.a[5]       =   -1.15795e-09;//                            
  row.a[6]       =    4.99614e-12;//                            
  row.a[7]       =   -8.74924e-15;//                            
  tableSet->AddAt(&row);// 8 -> dIfit
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

