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
  row.nrows	 =      nrows; // Correction/sigma versus LogTrackLength TPoints70BUGPHist118P06id_calib_pp_34
  row.npar	 =        -10; //
  row.a[0]	 =-1.65620e-01; //
  row.a[1]	 = 3.54080e-02;
  row.a[2]	 =-2.89898e-03;
  row.a[3]	 = 1.20076e-04;
  row.a[4]	 =-2.83622e-06;
  row.a[5]	 = 4.06459e-08;
  row.a[6]	 =-3.59990e-10;
  row.a[7]	 = 1.92537e-12;
  row.a[8]	 =-5.70090e-15;
  row.a[9]	 = 7.17853e-18;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; //
  row.nrows	 =      nrows; //
  row.npar	 =          5; //
  row.a[0]	 =  1.11775e+00; //
  row.a[1]	 = -1.24347e+00;
  row.a[2]	 =  5.86667e-01;
  row.a[3]	 = -1.21644e-01;
  row.a[4]	 =  9.15544e-03;
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
  row.nrows	 =      nrows; //TPointsBUGPHist118P06id_calib_pp_34.root
  row.npar	 =        -10; //
  row.a[0]	 = -3.43866e-01; //
  row.a[1]	 =  8.55971e-02;
  row.a[2]	 = -7.82843e-03;
  row.a[3]	 =  3.62079e-04;
  row.a[4]	 = -9.66474e-06;
  row.a[5]	 =  1.58494e-07;
  row.a[6]	 = -1.62510e-09;
  row.a[7]	 =  1.01663e-11;
  row.a[8]	 = -3.55071e-14;
  row.a[9]	 =  5.30771e-17;
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          6; //
  row.nrows	 =      nrows; //
  row.npar	 =          5; //
  row.min        = 3.21887582;
  row.max        = 4.82831374;
  row.a[0]	 =-8.27236e+00; //
  row.a[1]	 = 8.64426e+00;
  row.a[2]	 =-3.27303e+00;
  row.a[3]	 = 5.41237e-01;
  row.a[4]	 =-3.31537e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
    
