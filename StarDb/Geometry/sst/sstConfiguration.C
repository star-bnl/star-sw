TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/sst/.sstConfiguration/sstConfiguration Allocated rows: 1  Used rows: 1  Row size: 92 bytes
//  Table: sstConfiguration_st[0]--> sstConfiguration_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstConfiguration")) return 0;
sstConfiguration_st row;
St_sstConfiguration *tableSet = new St_sstConfiguration("sstConfiguration",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.nMaxSectors	 =          4; // 4 ;
    row.nMaxLadders	 =         20; // 20;
    row.nMaxWafers	 =        320; // 320;
    row.ladderIsPresent[0]	 =          1; // 1 means present, 0 means no present;
    row.ladderIsPresent[1]	 =          1;
    row.ladderIsPresent[2]	 =          1;
    row.ladderIsPresent[3]	 =          1;
    row.ladderIsPresent[4]	 =          1;
    row.ladderIsPresent[5]	 =          1;
    row.ladderIsPresent[6]	 =          1;
    row.ladderIsPresent[7]	 =          1;
    row.ladderIsPresent[8]	 =          1;
    row.ladderIsPresent[9]	 =          1;
    row.ladderIsPresent[10]	 =          1;
    row.ladderIsPresent[11]	 =          1;
    row.ladderIsPresent[12]	 =          1;
    row.ladderIsPresent[13]	 =          1;
    row.ladderIsPresent[14]	 =          1;
    row.ladderIsPresent[15]	 =          1;
    row.ladderIsPresent[16]	 =          1;
    row.ladderIsPresent[17]	 =          1;
    row.ladderIsPresent[18]	 =          1;
    row.ladderIsPresent[19]	 =          1;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
