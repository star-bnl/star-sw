TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/RunLog/onl/.starMagAvg/starMagAvg Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: starMagAvg_st[0]--> starMagAvg_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_starMagAvg")) return 0;
starMagAvg_st row;
St_starMagAvg *tableSet = new St_starMagAvg("starMagAvg",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   20060057; // run number  ;
    row.startRunTime	 = 1551475834; // unix time of entry  ;
    row.endRunTime	 = 1551475945; // unix time of entry  ;
    row.noEntries	 =          4; // no. entries used for averging ;
    row.current	 =   -4511.42; // averaged magnet current (- means B polarity)  ;
    row.rms	 =       0.18; // its rms ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
