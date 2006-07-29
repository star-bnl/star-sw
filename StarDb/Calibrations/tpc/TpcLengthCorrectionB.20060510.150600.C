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
  row.nrows	 =      nrows; // Correction/sigma versus LogTrackLength TPoints70BUGPHist115P06id_calib_pp_34
  row.npar	 =         -9; //
  row.a[0]	 =-6.81677e-02; //
  row.a[1]	 = 1.21783e-02;
  row.a[2]	 =-9.00945e-04;
  row.a[3]	 = 3.50367e-05;
  row.a[4]	 =-7.63604e-07;
  row.a[5]	 = 9.69917e-09;
  row.a[6]	 =-7.11898e-11;
  row.a[7]	 = 2.79201e-13;
  row.a[8]	 =-4.52313e-16;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; //
  row.nrows	 =      nrows; //
  row.npar	 =          5; //
  row.a[0]	 =  9.78451e-01; //
  row.a[1]	 = -1.02749e+00;
  row.a[2]	 =  4.73426e-01;
  row.a[3]	 = -9.71810e-02;
  row.a[4]	 =  7.28069e-03;
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
  row.nrows	 =      nrows; //TPointsBUGPHist115P06id_calib_pp_34.root
  row.npar	 =        -10; //
  row.a[0]	 =-2.64760e-02; //
  row.a[1]	 = 1.02130e-02;
  row.a[2]	 =-1.14672e-03;
  row.a[3]	 = 6.07097e-05;
  row.a[4]	 =-1.75933e-06;
  row.a[5]	 = 3.03903e-08;
  row.a[6]	 =-3.22409e-10;
  row.a[7]	 = 2.06393e-12;
  row.a[8]	 =-7.32747e-15;
  row.a[9]	 = 1.10913e-17;
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          6; //
  row.nrows	 =      nrows; //
  row.npar	 =          6; //
  row.a[0]	 = 4.28158e+00; //
  row.a[1]	 =-6.01236e+00;
  row.a[2]	 = 3.40448e+00;
  row.a[3]	 =-9.40302e-01;
  row.a[4]	 = 1.26284e-01;
  row.a[5]	 =-6.60814e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
    
