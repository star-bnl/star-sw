TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Geometry/tpc/.tpcPadConfig/tpcPadConfig Allocated rows: 1  Used rows: 1  Row size: 1392 bytes
  //  Table: tpcPadConfig_st[0]--> tpcPadConfig_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcPadConfig")) return 0;
  tpcPadConfig_st row;
  St_tpcPadConfig *tableSet = new St_tpcPadConfig("tpcPadConfig",1);
  Int_t i = 0;
  for (i = 1; i <=24; i++) row.itpc[i-1] = 1;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
