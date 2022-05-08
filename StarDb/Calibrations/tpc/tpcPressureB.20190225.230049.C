TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcPressureB/tpcPressureB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =            0; // ;
  row.idx	 =            1; // ;
  row.nrows	 =        nrows; // ;
  row.npar	 =            2; // ;
  row.a[0]	 =  1.98302e+01; // ;
  row.a[1]	 = -2.86577e+00;
 tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // keep it
  row.type	 =            0; // ;
  row.idx	 =            2; // ;
  row.nrows	 =        nrows; // ;
  row.npar	 =            2; // ;
  row.a[0]	 =  1.77769e+01; // ;
  row.a[1]	 = -2.56924e+00;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
