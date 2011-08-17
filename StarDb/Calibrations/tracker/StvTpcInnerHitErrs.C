TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcInnerHitErrs Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: StvHitErrs_st[0]--> StvHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvHitErrs")) return 0;
StvHitErrs_st row;
St_StvHitErrs *tableSet = new St_StvHitErrs("StvTpcInnerHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 1.000217e-08; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.01281067; // Intrinsic resolution, z direction			;
    row.widTrk	 = 0.001849165; // Track_width sigma**2					;
    row.thkDet	 = 0.08333333; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.001743006; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 0.00115734; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
