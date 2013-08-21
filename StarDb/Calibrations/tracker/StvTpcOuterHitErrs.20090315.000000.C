TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcOuterHitErrs Allocated rows: 1  Used rows: 1  Row size: 64 bytes
//  Table: StvTpcHitErrs_st[0]--> StvTpcHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvTpcHitErrs")) return 0;
StvTpcHitErrs_st row;
St_StvTpcHitErrs *tableSet = new St_StvTpcHitErrs("StvTpcOuterHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.001025376; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.009810519; // Intrinsic resolution, z direction			;
    row.thkDet	 =     3.8025; // detector thickness , not fitted			;
    row.yyDiff	 = 0.003210183; // Diffusion in XY direction				;
    row.zzDiff	 = 0.0009121676; // Diffusion in Z direction  				;
    row.yFact	 =  0.4309839; // Error factor in Y-direction 			;
    row.zFact	 =  0.4151657; // Error factor in Z-direction 			;
    row.zAB2	 = 5.0293e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
