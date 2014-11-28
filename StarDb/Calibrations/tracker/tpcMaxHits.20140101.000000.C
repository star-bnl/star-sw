TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tracker/.tpcMaxHits/tpcMaxHits Allocated rows: 1  Used rows: 1  Row size: 264 bytes
//  Table: tpcMaxHits_st[0]--> tpcMaxHits_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcMaxHits")) return 0;
tpcMaxHits_st row;
St_tpcMaxHits *tableSet = new St_tpcMaxHits("tpcMaxHits",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.maxSectorHits	 =      25000; // maximum hits in a sector ;
    row.maxBinZeroHits	 =          0; // maximum hits starting at time bin zero ;
 memcpy(&row.comment,"Introduce\x20maxBinZeroHits:\x20http://drupal.star.bnl.gov/STAR/blog/genevb/2011/mar/02/rt-ticket-2099",96);// considerations in determining max 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
