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
  row.npar	 =         -8; //
  row.a[0]	 =-5.49371e-02; //
  row.a[1]	 = 7.65299e-03;
  row.a[2]	 =-4.81105e-04;
  row.a[3]	 = 1.58392e-05;
  row.a[4]	 =-2.85904e-07;
  row.a[5]	 = 2.85780e-09;
  row.a[6]	 =-1.48106e-11;
  row.a[7]	 = 3.09094e-14;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; //
  row.nrows	 =      nrows; //
  row.npar	 =          4; //
  row.a[0]	 =  -1.22419e-01; //
  row.a[1]	 =   2.53431e-01;
  row.a[2]	 =  -7.76874e-02;
  row.a[3]	 =   6.94488e-03;
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
  row.npar	 =        -10; //
  row.a[0]	 =-9.51141e-02; //
  row.a[1]	 = 2.35524e-02;
  row.a[2]	 =-2.22359e-03;
  row.a[3]	 = 1.05643e-04;
  row.a[4]	 =-2.86746e-06;
  row.a[5]	 = 4.74331e-08;
  row.a[6]	 =-4.87746e-10;
  row.a[7]	 = 3.04808e-12;
  row.a[8]	 =-1.06107e-14;
  row.a[9]	 = 1.57917e-17;
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          6; //
  row.nrows	 =      nrows; //
  row.npar	 =          6; //
  row.a[0]	 =  4.95783e+00; //
  row.a[1]	 = -6.95396e+00;
  row.a[2]	 =  3.92131e+00;
  row.a[3]	 = -1.08056e+00;
  row.a[4]	 =  1.45249e-01;
  row.a[5]	 = -7.63440e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
    
