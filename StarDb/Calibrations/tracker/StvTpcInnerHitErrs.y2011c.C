TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StvTpcInnerHitErrs Allocated rows: 1  Used rows: 1  Row size: 72 bytes
//  Table: StvTpcHitErrs_st[0]--> StvTpcHitErrs_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_StvTpcHitErrs")) return 0;
StvTpcHitErrs_st row;
St_StvTpcHitErrs *tableSet = new St_StvTpcHitErrs("StvTpcInnerHitErrs",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.yErr	 = 0.004164817; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 0.01230661; // Intrinsic resolution, z direction			;
    row.thkDet	 =     1.3225; // detector thickness**2 , not fitted			;
    row.yyDiff	 = 0.005055965; // Diffusion in XY direction *yFactor			;
    row.zzDiff	 = 1.207996e-08; // Diffusion in Z direction  *ZFactor			;
    row.yzDiff	 = 2.078102e-05; // Diffusion in Z direction  *ZFactor			;
    row.yFact	 =  0.6018716; // Error factor in Y-direction 			;
    row.zFact	 =  0.7306963; // Error factor in Z-direction 			;
    row.zAB2	 = 5.0293e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
