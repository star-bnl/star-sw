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
//                                        hit errors are squared !
    row.coeff[0]	 =   0.066;    // u intrinsic;
    row.coeff[1]	 =   0.00012;  // u cross;
    row.coeff[2]	 =   0.0004;   // u drift;
    row.coeff[3]	 =   0.066;    // z intrinsic;
    row.coeff[4]	 =   0.00044;  // z dip;
    row.coeff[5]	 =   0.028;    // z drift;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
