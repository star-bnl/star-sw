TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.triggerInfo/triggerInfo Allocated rows: 1  Used rows: 1  Row size: 124 bytes
//  Table: triggerInfo_st[0]--> triggerInfo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_triggerInfo")) return 0;
triggerInfo_st row;
St_triggerInfo *tableSet = new St_triggerInfo("triggerInfo",3);
//
 memset(&row,0,tableSet->GetRowSize());
 memcpy(&row.name,"epoch\x00",5);// name  
 tableSet->AddAt(&row);
 tableSet->AddAt(&row);
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
