TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionB/TpcAdcCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
  //  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionB",3);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =         10; // ;
  row.idx	 =          1; // ;
  row.nrows	 =          2; // ;
  row.npar	 =          4; // ;
  row.OffSet	 =          0; // ;
  row.min	 =          3; // ;
  row.max	 =          9; // ;
  row.a[0]	 =   5.262643; // ;
  row.a[1]	 =   -1.85861;
  row.a[2]	 =   0.279676;
  row.a[3]	 = -0.0145679;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =         10; // ;
  row.idx	 =          2; // ;
  row.nrows	 =          2; // ;
  row.npar	 =          4; // ;
  row.OffSet	 =          0; // ;
  row.min	 =          3; // ;
  row.max	 =          9; // ;
  row.a[0]	 =   5.087566; // ;
  row.a[1]	 =   -1.91954;
  row.a[2]	 =   0.293305;
  row.a[3]	 = -0.0153733;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t i = 2; i < 50; i++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
