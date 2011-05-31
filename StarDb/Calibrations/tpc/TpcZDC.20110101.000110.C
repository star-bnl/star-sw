TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.TpcZDC/TpcZDC Allocated rows: 50  Used rows: 50  Row size: 120 bytes
  //  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZDC",2);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =           0; // Zdc3CGFRunXI10dev_calib_pp500_production_2011_ReversedFullField
  row.idx	 =           1; //
  row.nrows	 =           2; //
  row.npar	 =           2; //
  row.min        =         4.5;
  row.max        =         5.5;
  row.a[0]	 = 1.04877e-01; //
  row.a[1]	 =-2.05920e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =           0; //
  row.idx	 =           2; //
  row.nrows	 =           2; //
  row.min        =         4.5;
  row.max        =         5.5;
  row.npar	 =           5; //
  row.a[0]	 = 3.87119e-01; //
  row.a[1]	 =-7.61235e-02;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
