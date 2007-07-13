TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/RunLog/onl/.trigL3Expanded/trigL3Expanded Allocated rows: 10  Used rows: 10  Row size: 84 bytes
  //  Table: trigL3Expanded_st[0]--> trigL3Expanded_st[9]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_trigL3Expanded")) return 0;
  trigL3Expanded_st row;
  St_trigL3Expanded *tableSet = new St_trigL3Expanded("trigL3Expanded",10);
  //
 memset(&row,0,tableSet->GetRowSize());
 memcpy(&row.name,"epoch\x00",5);// StL2triggerResultType 
 for (Int_t i = 0; i < 10; i++)  tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
