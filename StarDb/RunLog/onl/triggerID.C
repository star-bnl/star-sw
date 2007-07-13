TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.triggerID/triggerID Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: triggerID_st[0]--> triggerID_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_triggerID")) return 0;
triggerID_st row;
St_triggerID *tableSet = new St_triggerID("triggerID",1);
//
memset(&row,0,tableSet->GetRowSize());
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
