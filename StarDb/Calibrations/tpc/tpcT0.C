TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tpc/.tpcT0/tpcT0 Allocated rows: 1  Used rows: 1  Row size: 72 bytes
//  Table: tpcT0_st[0]--> tpcT0_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcT0")) return 0;
  tpcT0_st row;
  St_tpcT0 *tableSet = new St_tpcT0("tpcT0",24);
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t sec = 1; sec <= 24; sec++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
