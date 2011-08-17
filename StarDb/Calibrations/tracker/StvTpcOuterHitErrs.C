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
    row.yErr	 = 0.002714903; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 1.000285e-08; // Intrinsic resolution, z direction			;
    row.widTrk	 = 0.00974292; // Track_width sigma**2					;
    row.thkDet	 = 0.08333333; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.0006636942; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 1.000323e-08; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
