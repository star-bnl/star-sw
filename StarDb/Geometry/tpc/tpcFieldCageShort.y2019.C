TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/tpc/.tpcFieldCageShort/tpcFieldCageShort Allocated rows: 10  Used rows: 10  Row size: 20 bytes
//  Table: tpcFieldCageShort_st[0]--> tpcFieldCageShort_st[9]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcFieldCageShort")) return 0;
  tpcFieldCageShort_st row;
  St_tpcFieldCageShort *tableSet = new St_tpcFieldCageShort("tpcFieldCageShort",10);
  // No short
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t i = 0; i < 10; i++) tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
