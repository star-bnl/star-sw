TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/StarDb/.data/StarDb/Conditions/trg/trgTimeOffset Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: trgTimeOffset_st[0]--> trgTimeOffset_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_trgTimeOffset")) return 0;
trgTimeOffset_st row;
St_trgTimeOffset *tableSet = new St_trgTimeOffset("trgTimeOffset",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.offset	 =        2.05; // standard trigger offset in micro-seconds  ;
    row.laserOffset	 =        0.95; // laser trigger offset in micro-seconds  ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
