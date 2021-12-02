TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tracker/.iTPCHitError/iTPCHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("iTPCHitError",1);
//
 Double_t scale = 0.5/0.335; // strip widths
memset(&row,0,tableSet->GetRowSize());
    row.coeff[0]	 =     0.0004 *scale*scale; // *;
    row.coeff[1]	 = 0.002045093*scale*scale;
    row.coeff[2]	 =  0.0355285 *scale*scale;
    row.coeff[3]	 = 0.00122247;
    row.coeff[4]	 = 0.005828129;
    row.coeff[5]	 = 0.02526806;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
