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
  row.nrows	 =      nrows; // Correction/sigma versus LogTrackLength TPoints70BUGPTPoints70BUGPHist126P06id_dedx
  row.npar	 =          -9; //
  row.a[0]	 =-9.91189e-02; //
  row.a[1]	 = 1.94789e-02;
  row.a[2]	 =-1.44358e-03;
  row.a[3]	 = 5.27344e-05;
  row.a[4]	 =-1.05744e-06;
  row.a[5]	 = 1.22523e-08;
  row.a[6]	 =-8.15661e-11;
  row.a[7]	 = 2.88244e-13;
  row.a[8]	 =-4.16598e-16;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; //
  row.nrows	 =      nrows; //
  row.npar	 =          6; //
  row.min        =        2.3;
  row.max        =      4.787;
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
  row.nrows	 =      nrows; //TPointsBUGPTPoints70BUGPHist126P06id_dedx.root
  row.npar	 =         -10; //
  row.a[0]	 =-7.55315e-01; //
  row.a[1]	 = 1.61241e-01;
  row.a[2]	 =-1.33861e-02;
  row.a[3]	 = 5.81156e-04;
  row.a[4]	 =-1.48521e-05;
  row.a[5]	 = 2.35884e-07;
  row.a[6]	 =-2.35840e-09;
  row.a[7]	 = 1.44467e-11;
  row.a[8]	 =-4.95378e-14;
  row.a[9]	 = 7.28271e-17;
  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          6; //
  row.nrows	 =      nrows; //
  row.npar	 =          6; //
  row.min        =      2.398;
  row.max        =      4.787;
  row.a[0]	 =  2.89057e+01; //
  row.a[1]	 = -4.05691e+01;
  row.a[2]	 =  2.25297e+01;
  row.a[3]	 = -6.16249e+00;
  row.a[4]	 =  8.30560e-01;
  row.a[5]	 = -4.41793e-02;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
    
