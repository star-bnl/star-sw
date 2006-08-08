TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.TpcLengthCorrectionB/TpcLengthCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
  //  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          1; //
  row.nrows	 =      nrows; // Correction/sigma versus LogTrackLength TPoints70BUGPHist115P06id_calib_pp_12
  row.npar	 =         -10; //
  row.a[0]	 =-1.13664e-01; //
  row.a[1]	 = 2.17324e-02;
  row.a[2]	 =-1.59714e-03;
  row.a[3]	 = 5.58547e-05;
  row.a[4]	 =-1.01453e-06;
  row.a[5]	 = 9.21780e-09;
  row.a[6]	 =-2.48202e-11;
  row.a[7]	 =-2.31317e-13;
  row.a[8]	 = 1.96692e-15;
  row.a[9]	 =-4.38075e-18;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; //
  row.nrows	 =      nrows; //
  row.npar	 =          6; //
  row.a[0]	 =   2.74510e+00; //
  row.a[1]	 =  -3.64943e+00;
  row.a[2]	 =   1.98889e+00;
  row.a[3]	 =  -5.25204e-01;
  row.a[4]	 =   6.66543e-02;
  row.a[5]	 =  -3.24793e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          3; //
  row.nrows	 =      nrows; //
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          4; //
  row.nrows	 =      nrows; //
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          5; // 
  row.nrows	 =      nrows; //TPointsBUGPHist115P06id_calib_pp_12.root
  row.npar	 =         -10; //
  row.a[0]	 =-7.90235e-01; //
  row.a[1]	 = 1.72553e-01;
  row.a[2]	 =-1.46767e-02;
  row.a[3]	 = 6.48297e-04;
  row.a[4]	 =-1.67876e-05;
  row.a[5]	 = 2.69345e-07;
  row.a[6]	 =-2.71406e-09;
  row.a[7]	 = 1.67247e-11;
  row.a[8]	 =-5.76062e-14;
  row.a[9]	 = 8.49641e-17;
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          6; //
  row.nrows	 =      nrows; //
  row.npar	 =          4; //
  row.min        =  3.4011973;
  row.max        =  4.7874917;
  row.a[0]	 =  2.99505e+00; //
  row.a[1]	 = -1.92951e+00;
  row.a[2]	 =  4.31085e-01;
  row.a[3]	 = -3.25150e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
    
