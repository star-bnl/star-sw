TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.ist3HitError/ist3HitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("StvIstHitErrs",1);
//
 Double_t sigma_z = 1.11; 
 Double_t sigma_y = 0.0017;
memset(&row,0,tableSet->GetRowSize());
//                                        hit errors are squared !
    row.coeff[0]	 =   sigma_y*sigma_y;     // y intrinsic;
    row.coeff[1]	 =   sigma_z*sigma_z;     // z intrinsic;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
