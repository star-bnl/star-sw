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
    row.yErr	 = 0.007265169; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.007309109; // Intrinsic resolution, z direction			;
    row.widTrk	 = 2.851396e-05; // Track_width sigma**2					;
    row.thkDet	 = 0.04400726; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.001365866; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 0.001803627; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
