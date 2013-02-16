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
    row.yErr	 =      1e-08; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.0002120533; // Intrinsic resolution, z direction			;
    row.widTrk	 = 8.978132e-06; // Track_width sigma**2					;
    row.thkDet	 = 0.01522316; // (Thickness of detector plane)**2/12			;
    row.yDiff	 = 0.0008945257; // Y Drift dependent diffusion resolution, z direction	;
    row.zDiff	 = 0.002879677; // Z Drift dependent diffusion resolution, z direction	;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
