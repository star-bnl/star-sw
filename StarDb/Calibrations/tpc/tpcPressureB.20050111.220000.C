TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcPressureB/tpcPressureB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
  //  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          1; // ;  PressureCGFHist003P05id.root
  row.nrows	 =          2; // ;
  row.npar	 =          2; // ;
  row.a[0]	 =  22.176274; // ;
  row.a[1]	 =  -3.202996;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; // ;
  row.nrows	 =          2; // ;
  row.npar	 =          2; // ;
  row.a[0]	 =  25.607180; // ;
  row.a[1]	 =  -3.698494;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
