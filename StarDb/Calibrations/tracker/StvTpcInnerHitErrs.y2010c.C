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
    row.yErr	 = 3.775685e-08; // Intrinsic resolution, padrow or Y direction		;
    row.zErr	 = 1.67014e-06; // Intrinsic resolution, z direction			;
    row.thkDet	 =     1.3225; // detector thickness , not fitted			;
    row.yyDiff	 = 0.007464294; // Diffusion in XY direction				;
    row.zzDiff	 = 0.01401472; // Diffusion in Z direction  				;
    row.yFact	 =  0.4522974; // Error factor in Y-direction 			;
    row.zFact	 =  0.5037845; // Error factor in Z-direction 			;
    row.zAB2	 = 5.0293e-06; // Constant member in Z direction (a*b)**2		;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
