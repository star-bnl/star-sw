TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.tpcOuterHitError/tpcOuterHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("tpcOuterHitError",1);
//
memset(&row,0,tableSet->GetRowSize());
//                                        hit errors are squared !
    row.coeff[0]	 =   0.000310687;    	// u intrinsic;
    row.coeff[1]	 =   0.00210523;    	// u cross;
    row.coeff[2]	 =   0.0318109;     	// u drift;
    row.coeff[3]	 =   0.00126539;    	// z intrinsic;
    row.coeff[4]	 =   0.0074283;    	// z dip;
    row.coeff[5]	 =   0.0295562;     	// z drift;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
