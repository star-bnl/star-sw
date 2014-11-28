TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcInnerPromptErrs Allocated rows: 1  Used rows: 1  Row size: 64 bytes
//  Table: StvTpcPromptErrs_st[0]--> StvTpcPromptErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvHitErrs")) return 0;
StvHitErrs_st row;
St_StvHitErrs *tableSet = new St_StvHitErrs("StvTpcInnerPromptErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 4e-2; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 4e-2; // Intrinsic resolution, z direction			;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
