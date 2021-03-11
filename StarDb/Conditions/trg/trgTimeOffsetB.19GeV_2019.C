TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Conditions/trg/.trgTimeOffsetB/trgTimeOffsetB Allocated rows: 1  Used rows: 1  Row size: 12 bytes
//  Table: trgTimeOffset_st[0]--> trgTimeOffset_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_trgTimeOffset")) return 0;
trgTimeOffset_st row;
St_trgTimeOffset *tableSet = new St_trgTimeOffset("trgTimeOffsetB",1);
//
memset(&row,0,tableSet->GetRowSize());
 row.offset	         =     2.3652; // standard trigger offset in micro-seconds  ;
    row.laserOffset	 =      1.469; // laser trigger offset in micro-seconds  ;
    row.laserOffsetW	 =     0.0429; // laser extra trigger offset for West laser ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
