TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// someHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("tpcOuterHitError",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.coeff[0]	 = 0.0008832995; // *;
    row.coeff[1]	 = 0.0006812068;
    row.coeff[2]	 = 0.05740916;
    row.coeff[3]	 = 0.007070432;
    row.coeff[4]	 = 0.0005533033;
    row.coeff[5]	 = 0.05240276;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
