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
    row.yErr	 = 2.436651e-07; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.002303206; // Intrinsic resolution, z direction			;
    row.widTrk	 = 5.333157e-05; // Track_width sigma**2					;
    row.thkDet	 = 0.04339923; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.006561827; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 0.01321465; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
