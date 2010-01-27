TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// someHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("someHitError",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.coeff[0]	 = 0.001183682; // *;
    row.coeff[1]	 = 0.00065211;
    row.coeff[2]	 = 0.05818009;
    row.coeff[3]	 = 0.01016755;
    row.coeff[4]	 = 0.000509845;
    row.coeff[5]	 =  0.0506341;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
