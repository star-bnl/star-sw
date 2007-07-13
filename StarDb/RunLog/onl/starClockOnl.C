TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.starClockOnl/starClockOnl Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: starClockOnl_st[0]--> starClockOnl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_starClockOnl")) return 0;
starClockOnl_st row;
St_starClockOnl *tableSet = new St_starClockOnl("starClockOnl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =    0; // run number  ;
    row.time	         =    0; // unix time of entry  ;
    row.frequency	 =    9383160; // frequency in Hz  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
