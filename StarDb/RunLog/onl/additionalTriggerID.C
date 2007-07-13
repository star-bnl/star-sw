TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.additionalTriggerID/additionalTriggerID Allocated rows: 320  Used rows: 320  Row size: 36 bytes
//  Table: additionalTriggerID_st[0]--> additionalTriggerID_st[319]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_additionalTriggerID")) return 0;
  additionalTriggerID_st row;
  St_additionalTriggerID *tableSet = new St_additionalTriggerID("additionalTriggerID",320);
  //
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t i = 0; i < 320; i++) tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
