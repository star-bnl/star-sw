TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/ftpc/.ftpcAsicMap/ftpcAsicMap Allocated rows: 1  Used rows: 1  Row size: 4 bytes
//  Table: ftpcAsicMap_st[0]--> ftpcAsicMap_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcAsicMap")) return 0;
ftpcAsicMap_st row;
St_ftpcAsicMap *tableSet = new St_ftpcAsicMap("ftpcAsicMap",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.EastIsInverted	 =          1; // of the STAR coordinate system for FTPC East;
    row.Asic2EastNotInverted	 =          1; // the error was correct before the Y2003 data taking started ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
