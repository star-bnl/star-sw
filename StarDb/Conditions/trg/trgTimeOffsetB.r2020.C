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
 row.offset	 =   2.505249 -0.1068 -0.0080 +3.58295e-01-0.0324796 -0.00173833; // standard trigger offset in micro-seconds  ; C: dT->Fit("pol2") 
    row.laserOffset	 =      1.469; // laser trigger offset in micro-seconds  ;
    row.laserOffsetW	 =     0.0429; // laser extra trigger offset for West laser ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
