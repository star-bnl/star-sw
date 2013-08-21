TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcInnerHitErrs Allocated rows: 1  Used rows: 1  Row size: 64 bytes
//  Table: StvTpcHitErrs_st[0]--> StvTpcHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvTpcHitErrs")) return 0;
StvTpcHitErrs_st row;
St_StvTpcHitErrs *tableSet = new St_StvTpcHitErrs("StvTpcInnerHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.003112073; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.006408807; // Intrinsic resolution, z direction			;
    row.thkDet	 =     1.3225; // detector thickness , not fitted			;
    row.yyDiff	 = 0.001792184; // Diffusion in XY direction				;
    row.zzDiff	 = 0.009125377; // Diffusion in Z direction  				;
    row.yFact	 =   0.651482; // Error factor in Y-direction 			;
    row.zFact	 =  0.5302516; // Error factor in Z-direction 			;
    row.zAB2	 = 5.0293e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
