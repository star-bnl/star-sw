TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/ftpc/.ftpcVoltage/ftpcVoltage Allocated rows: 1  Used rows: 1  Row size: 116 bytes
//  Table: ftpcVoltage_st[0]--> ftpcVoltage_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcVoltage")) return 0;
ftpcVoltage_st row;
St_ftpcVoltage *tableSet = new St_ftpcVoltage("ftpcVoltage",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =    4129002; // ;
    row.cathodeVEast	 =       9997; // ;
    row.cathodeVWest	 =       9997; // ;
    row.anodeV1East	 =       16.2; // ;
    row.anodeV2East	 =       15.9; // ;
    row.anodeV3East	 =         17; // ;
    row.anodeV4East	 =       16.1; // ;
    row.anodeV5East	 =       16.5; // ;
    row.anodeV6East	 =       16.2; // ;
    row.anodeV1West	 =         15; // ;
    row.anodeV2West	 =       14.7; // ;
    row.anodeV3West	 =         16; // ;
    row.anodeV4West	 =       14.9; // ;
    row.anodeV5West	 =       12.9; // ;
    row.anodeV6West	 =       13.4; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
