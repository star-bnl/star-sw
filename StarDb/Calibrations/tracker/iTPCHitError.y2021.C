TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// someHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("iTPCHitError",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.coeff[0]	 =     0.0004; // *;
    row.coeff[1]	 = 0.001354549;
    row.coeff[2]	 = 0.03825068;
    row.coeff[3]	 = 0.00440583;
    row.coeff[4]	 = 0.005827833;
    row.coeff[5]	 = 0.02857583;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
