TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/StarDb/.data/StarDb/Conditions/trg/trgTimeOffset Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: trgTimeOffset_st[0]--> trgTimeOffset_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_trgTimeOffset")) return 0;
trgTimeOffset_st row;
St_trgTimeOffset *tableSet = new St_trgTimeOffset("trgTimeOffsetB",1);
//
 memset(&row,0,tableSet->GetRowSize()); // FF
 row.offset	         =   2.18147349 + 0.2186; //2.1815;//     2.05; // standard trigger offset in micro-seconds  ;
 row.laserOffset	 =   1.4878 - 0.0019;  //t0East
 row.laserOffsetW	 =   0.0028 + 0.0019 + -0.009 + 0.0099;  // -t0East - t0West
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
