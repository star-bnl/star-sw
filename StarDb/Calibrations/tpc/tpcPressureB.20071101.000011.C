TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcPressureB/tpcPressureB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
  //  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          1; // ;  PressureCGFRunVIII11P08ic_production_dAu2008_ReversedFullField
  row.nrows	 =          1; // ;
  row.npar	 =          2; // ;
  row.a[0]	 =  2.22629e+01; // ;
  row.a[1]	 = -3.21543e+00;
  tableSet->AddAt(&row);
 // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
