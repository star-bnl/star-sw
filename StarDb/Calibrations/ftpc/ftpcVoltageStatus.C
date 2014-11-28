TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/ftpc/.ftpcVoltageStatus/ftpcVoltageStatus Allocated rows: 1  Used rows: 1  Row size: 20 bytes
//  Table: ftpcVoltageStatus_st[0]--> ftpcVoltageStatus_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_ftpcVoltageStatus")) return 0;
ftpcVoltageStatus_st row;
St_ftpcVoltageStatus *tableSet = new St_ftpcVoltageStatus("ftpcVoltageStatus",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =          0; // ;
    row.startStatusTime	 =          0; // ;
    row.endStatusTime	 =          0; // ;
    row.statusWest	 =          0; // ;
    row.statusEast	 =          0; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
