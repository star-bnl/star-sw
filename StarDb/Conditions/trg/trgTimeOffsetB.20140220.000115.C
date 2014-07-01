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
 memset(&row,0,tableSet->GetRowSize()); // RF
 row.offset	         =   2.18147349; //2.1815;//     2.05; // standard trigger offset in micro-seconds  ;
 row.laserOffset	 =   1.4967 - 0.0182; //   t0east
 row.laserOffsetW	 =  -1.9895 + 0.0182 + 0.0064; // - t0east  - t0west
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
