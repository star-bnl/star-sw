TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.tpcStatus/tpcStatus Allocated rows: 1  Used rows: 1  Row size: 72 bytes
  //  Table: tpcStatus_st[0]--> tpcStatus_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcStatus")) return 0;
  tpcStatus_st row;
  St_tpcStatus *tableSet = new St_tpcStatus("tpcStatus",1);
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
