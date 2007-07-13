TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/rich/.richvoltages/richvoltages Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: richvoltages_st[0]--> richvoltages_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_richvoltages")) return 0;
richvoltages_st row;
St_richvoltages *tableSet = new St_richvoltages("richvoltages",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =          0; // ;
    row.startStatusTime	 =          0; // ;
    row.endStatusTime	 =          0; // ;
    row.status	 =          0; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
