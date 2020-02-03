TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/RunLog/onl/.starMagOnl/starMagOnl Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: starMagOnl_st[0]--> starMagOnl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_starMagOnl")) return 0;
starMagOnl_st row;
St_starMagOnl *tableSet = new St_starMagOnl("starMagOnl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   21030024; // run number  ;
    row.time	 = 1580416177; // unix time of entry  ;
    row.current	 =    -4511.6; // magnet current (- means B polarity)  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
