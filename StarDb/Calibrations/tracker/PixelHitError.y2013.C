TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.ssdHitError/ssdHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("PixelHitError",1);
//
 Double_t sigma_x = 0.00086; //  8.6 mkm
 Double_t sigma_z = 0.00086; // 8.6 mkm
memset(&row,0,tableSet->GetRowSize());
//                                        hit errors are squared !
    row.coeff[0]	 =   sigma_x*sigma_x;     // u intrinsic;
    row.coeff[1]	 =   0.0;      // u cross;
    row.coeff[2]	 =   0.0;      // u drift;
    row.coeff[3]	 =   sigma_z*sigma_z
;     // z intrinsic;
    row.coeff[4]	 =   0.0;      // z dip;
    row.coeff[5]	 =   0.0;      // z drift;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
