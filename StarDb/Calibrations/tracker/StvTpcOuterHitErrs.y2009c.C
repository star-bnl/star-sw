TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcOuterHitErrs Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: StvHitErrs_st[0]--> StvHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvHitErrs")) return 0;
StvHitErrs_st row;
St_StvHitErrs *tableSet = new St_StvHitErrs("StvTpcOuterHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.000295596; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.02294365; // Intrinsic resolution, z direction			;
    row.widTrk	 = 2.385502e-05; // Track_width sigma**2					;
    row.thkDet	 = 0.04000728; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.001283351; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 0.00023575; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
