TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionB/TpcAdcCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionB",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         10; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          8; // ;
    row.OffSet	 =     20.956; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]	 =   -64.3213; // ;
    row.a[1]	 =    71.0178;
    row.a[2]	 =    -32.622;
    row.a[3]	 =    8.15578;
    row.a[4]	 =    -1.1987;
    row.a[5]	 =   0.103617;
    row.a[6]	 = -0.00488051;
    row.a[7]	 = 9.67008e-05;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         10; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          7; // ;
    row.OffSet	 =     32.849; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]	 =    42.5414; // ;
    row.a[1]	 =   -37.8599;
    row.a[2]	 =    14.1435;
    row.a[3]	 =   -2.79842;
    row.a[4]	 =   0.309112;
    row.a[5]	 = -0.0180676;
    row.a[6]	 = 0.000436527;
    row.a[7]	 =          0;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
