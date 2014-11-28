TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/ftpc/.ftpcClusterGeom/ftpcClusterGeom Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: ftpcClusterGeom_st[0]--> ftpcClusterGeom_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcClusterGeom")) return 0;
ftpcClusterGeom_st row;
St_ftpcClusterGeom *tableSet = new St_ftpcClusterGeom("ftpcClusterGeom",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.minTimebin	 =        135; // ;
    row.minTimebinMed	 =         50; // ;
    row.minTimebinOut	 =          5; // ;
    row.maxTimelength	 =        100; // ;
    row.maxTimelengthMed	 =         30; // ;
    row.maxTimelengthOut	 =         10; // ;
    row.maxPadlength	 =        100; // ;
    row.maxPadlengthMed	 =         30; // ;
    row.maxPadlengthOut	 =         10; // ;
    row.deltaTime	 =          2; // ;
    row.deltaPad	 =          2; // ;
    row.minChargeWindow	 =         30; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
