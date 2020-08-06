TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcLengthCorrectionB/TpcLengthCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",6);
//
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          6; // ;
    row.npar	 =         -9; // ;
    row.OffSet	 =          0; // ;
    row.min	 =        2.3; // ;
    row.max	 =        4.7; // ;
    row.a[0]	 = -0.0966152; // ;
    row.a[1]	 =  0.0195722;
    row.a[2]	 = -0.00146613;
    row.a[3]	 = 5.44171e-05;
    row.a[4]	 = -1.10927e-06;
    row.a[5]	 = 1.30748e-08;
    row.a[6]	 = -8.88083e-11;
    row.a[7]	 = 3.22147e-13;
    row.a[8]	 = -4.82477e-16;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          6; // ;
    row.npar	 =          4; // ;
    row.OffSet	 =          0; // ;
    row.min	 =        2.3; // ;
    row.max	 =        4.7; // ;
    row.a[0]	 =  -0.314706; // ;
    row.a[1]	 =   0.404681;
    row.a[2]	 =  -0.116752;
    row.a[3]	 =  0.0101701;
    row.a[4]	 =          0;
    row.a[5]	 =          0;
    row.a[6]	 =          0;
    row.a[7]	 =          0;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          3; // ;
    row.nrows	 =          6; // ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          4; // ;
    row.nrows	 =          6; // ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          5; // ;
    row.nrows	 =          6; // ;
    row.npar	 =        -10; // ;
    row.OffSet	 =          0; // ;
    row.min	 =        2.3; // ;
    row.max	 =        4.7; // ;
    row.a[0]	 = -0.00624201; // ;
    row.a[1]	 =  0.0130656;
    row.a[2]	 = -0.00169159;
    row.a[3]	 = 9.15113e-05;
    row.a[4]	 = -2.65767e-06;
    row.a[5]	 = 4.60824e-08;
    row.a[6]	 = -4.94144e-10;
    row.a[7]	 = 3.22124e-12;
    row.a[8]	 = -1.1716e-14;
    row.a[9]	 = 1.82422e-17;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          6; // ;
    row.nrows	 =          6; // ;
    row.npar	 =          4; // ;
    row.OffSet	 =          0; // ;
    row.min	 =        2.3; // ;
    row.max	 =        4.7; // ;
    row.a[0]	 =  -0.533286; // ;
    row.a[1]	 =   0.580867;
    row.a[2]	 =  -0.164059;
    row.a[3]	 =  0.0143814;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
