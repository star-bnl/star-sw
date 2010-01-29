TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Correction/sigma versus LogTrackLength
  row.idx        =            1; // TPoints70BUGPRunIX63P09if_calibV
  row.npar       =           -8;
  row.max        =  135.;
  row.a[0]	 =  7.52480e-02;//
  row.a[1]	 = -1.89069e-03;//
  row.a[2]	 =  1.69860e-05;//
  row.a[3]	 =  1.83989e-06;//
  row.a[4]	 = -6.79537e-08;//
  row.a[5]	 =  9.55840e-10;//
  row.a[6]	 = -6.04451e-12;//
  row.a[7]	 =  1.43292e-14;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows;
  row.idx        =            2;// TPoints70BUGPRunIX63P09if_calibV
  row.npar       =            3;// 
  row.a[0]	 =  4.25896e-01; 
  row.a[1]	 = -1.31381e-01;
  row.a[2]	 =  1.18156e-02;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  tableSet->AddAt(&row);// 3 -> sigma.I60
  row.nrows      =        nrows;
  row.idx        =            5;// TPointsBUGPRunIX63P09if_calibV
  row.npar       =           -9;// 
  row.a[0]	 = -1.50267e-01;//
  row.a[1]	 =  3.31455e-02;//
  row.a[2]	 = -2.27442e-03;//
  row.a[3]	 =  8.12707e-05;//
  row.a[4]	 = -1.67780e-06;//
  row.a[5]	 =  2.06943e-08;//
  row.a[6]	 = -1.50224e-10;//
  row.a[7]	 =  5.91498e-13;//
  row.a[8]	 = -9.73991e-16;//
  tableSet->AddAt(&row);// 4 -> I  // 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =            6;// TPointsBUGPRunIX63P09if_calibV
  row.npar       =            6;// 
  row.a[0]	 =   7.03490e+00; 
  row.a[1]	 =  -9.73663e+00;
  row.a[2]	 =   5.45548e+00;
  row.a[3]	 =  -1.50918e+00;
  row.a[4]	 =   2.05208e-01;
  row.a[5]	 =  -1.09714e-02;
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

