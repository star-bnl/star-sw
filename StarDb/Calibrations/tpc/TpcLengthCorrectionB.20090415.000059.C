TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunIX57P09ig_calibAB + TPoints70BGPRunIX59P09ig_calibAB
  row.npar       =           10;
  row.max        = TMath::Log(140);
  row.a[0]	 =  1.15055e+02-2.74463e-01;//
  row.a[1]	 = -1.84893e+02+2.53726e-01;//
  row.a[2]	 =  1.10322e+02-8.39829e-02;//
  row.a[3]	 = -2.55383e+01+1.13485e-02;//
  row.a[4]	 = -9.89301e-01-4.67518e-04;//
  row.a[5]	 =  1.28405e+00;//
  row.a[6]	 = -1.69854e-02;//
  row.a[7]	 = -6.92243e-02;//
  row.a[8]	 =  1.24469e-02;//
  row.a[9]	 = -6.85668e-04;//
 tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BGPRunIX59P09ig_calibAB
  row.npar       =            6;// 
  row.a[0]	 =  7.03103e+00; 
  row.a[1]	 = -9.55597e+00;
  row.a[2]	 =  5.25598e+00;
  row.a[3]	 = -1.42622e+00;
  row.a[4]	 =  1.89948e-01;
  row.a[5]	 = -9.92861e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      =        nrows;
  row.idx        =            5;       // TPointsBUGPRunIX57P09ig_calibAB + TPointsBGPRunIX59P09ig_calibAB
  row.npar       =          -10;// 
  row.a[0]	 = -4.13593e-01+6.38292e-04;//
  row.a[1]	 =  7.79192e-02-3.74381e-06;//
  row.a[2]	 = -5.55183e-03-5.45136e-07;//
  row.a[3]	 =  2.04571e-04+5.51113e-09;//
  row.a[4]	 = -4.39234e-06;//
  row.a[5]	 =  5.81842e-08;//
  row.a[6]	 = -4.82792e-10;//
  row.a[7]	 =  2.44645e-12;//
  row.a[8]	 = -6.92571e-15;//
  row.a[9]	 =  8.39693e-18;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =         nrows;
  row.npar       =            6;// TPointsBGPRunIX59P09ig_calibAB
  row.a[0]	 =   7.34966e+00; 
  row.a[1]	 =  -1.02289e+01;
  row.a[2]	 =   5.70627e+00;
  row.a[3]	 =  -1.56104e+00;
  row.a[4]	 =   2.08791e-01;
  row.a[5]	 =  -1.09324e-02;
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

