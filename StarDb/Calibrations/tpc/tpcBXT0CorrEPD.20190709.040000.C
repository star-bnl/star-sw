TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcPressureB/tpcPressureB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
	if (!TClass::GetClass("St_tpcBXT0Corr")) return 0;
	tpcBXT0Corr_st row;
	St_tpcBXT0Corr *tableSet = new St_tpcBXT0Corr("tpcBXT0CorrEPD",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.type	 =          0; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          1; // ;
    row.npar	 =          1; // ;
    row.OffSet	 =          0; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]	 = 0; // This the parameter related to dz introduced by shift of timeBucket by one unit (property of the beam frequency);
    row.a[1]	 = 0; // This is the general offset for every event set to the mean of the deltaZ distribution;
    row.a[2]	 = 0; // This is the offset of the linear corelation of the deltaZ vs. epdMacTac;
    row.a[3]	 = 0; // This is the slope of the linear corelation of the deltaZ vs. epdMacTac;
    row.a[4]	 =          0;
    row.a[5]	 =          0;
    row.a[6]	 =          0;
    row.a[7]	 =          0;
    row.a[8]	 =          0;
    row.a[9]	 =          0;
    strcpy(row.comment,"test");
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
