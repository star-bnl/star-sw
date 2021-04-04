TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/RunLog/onl/.starTriggerDelay/starTriggerDelay Allocated rows: 1  Used rows: 1  Row size: 16 bytes
  //  Table: starTriggerDelay_st[0]--> starTriggerDelay_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_starTriggerDelay")) return 0;
  starTriggerDelay_st row;
  St_starTriggerDelay *tableSet = new St_starTriggerDelay("starTriggerDelay",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.clocks     = 17; // from Run XIX + XX + XXI scan 
  row.tZero	 = 0.435; // -"-
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
