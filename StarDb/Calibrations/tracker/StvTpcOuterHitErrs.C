TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcOuterHitErrs Allocated rows: 1  Used rows: 1  Row size: 56 bytes
//  Table: StvHitErrs_st[0]--> StvHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvHitErrs")) return 0;
StvHitErrs_st row;
St_StvHitErrs *tableSet = new St_StvHitErrs("StvTpcOuterHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.003025857; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.006877033; // Intrinsic resolution, z direction			;
    row.yDiff	 = 0.0003152263; // Diffusion in XY direction				;
    row.zDiff	 =      1e-08; // Diffusion in Z direction				;
    row.yThkDet	 =  0.8378609; // Effective detector thickness**2 for Y err 		;
    row.zThkDet	 =  0.6543077; // Effective detector thickness**2 for Z err		;
    row.zAB2	 = 5.127968e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
