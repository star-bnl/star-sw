TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.tpcInnerHitError/tpcInnerHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("tpcInnerHitError",1);
// 
memset(&row,0,tableSet->GetRowSize());
// 0.00168243, 0.005233, 0.05753410, 0.00312735, 0.015106, 0.02438060
//                                        hit errors are squared !
    row.coeff[0]	 =   0.00011532;    	// u intrinsic;
    row.coeff[1]	 =   0.00245802;  	// u cross;
    row.coeff[2]	 =   0.011586;   	// u drift;
    row.coeff[3]	 =   0.000886716;    	// z intrinsic;
    row.coeff[4]	 =   0.00920188;  	// z dip;
    row.coeff[5]	 =   0.0125244;    	// z drift;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
