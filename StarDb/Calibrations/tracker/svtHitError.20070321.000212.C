TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// someHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
  HitError_st row;
  St_HitError *tableSet = new St_HitError("someHitError",1);
  //
  Double_t sigmaX = 54e-4;
  Double_t sigmaZ = 44e-4;
  memset(&row,0,tableSet->GetRowSize());
  row.coeff[0]	 = sigmaX*sigmaX; // *;
  row.coeff[1]	 =          0;
  row.coeff[2]	 =          0;
  row.coeff[3]	 = sigmaZ*sigmaZ;
  row.coeff[4]	 =          0;
  row.coeff[5]	 =          0;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
