TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcInnerHitErrs Allocated rows: 1  Used rows: 1  Row size: 56 bytes
//  Table: StvHitErrs_st[0]--> StvHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvHitErrs")) return 0;
StvHitErrs_st row;
St_StvHitErrs *tableSet = new St_StvHitErrs("StvTpcInnerHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.005388335; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.01163993; // Intrinsic resolution, z direction			;
    row.yDiff	 = 0.001356777; // Diffusion in XY direction				;
    row.zDiff	 = 5.178958e-07; // Diffusion in Z direction				;
    row.yThkDet	 =  0.4863147; // Effective detector thickness**2 for Y err 		;
    row.zThkDet	 =  0.4084341; // Effective detector thickness**2 for Z err		;
    row.zAB2	 = 3.340908e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
