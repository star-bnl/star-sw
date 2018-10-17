TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcPressureB/tpcPressureB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          2; // ;
    row.OffSet	 =          0; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]	 =   21.62552; // ;
    row.a[1]	 =  -3.124518;
    row.a[2]	 =          0;
    row.a[3]	 =          0;
    row.a[4]	 =          0;
    row.a[5]	 =          0;
    row.a[6]	 =          0;
    row.a[7]	 =          0;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          2; // ;
    row.OffSet	 =          0; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]	 =   14.75337; // ;
    row.a[1]	 =  -2.132122;
    row.a[2]	 =          0;
    row.a[3]	 =          0;
    row.a[4]	 =          0;
    row.a[5]	 =          0;
    row.a[6]	 =          0;
    row.a[7]	 =          0;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
