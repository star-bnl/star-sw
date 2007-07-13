TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.trigPrescales/trigPrescales Allocated rows: 1  Used rows: 1  Row size: 20 bytes
//  Table: trigPrescales_st[0]--> trigPrescales_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_trigPrescales")) return 0;
trigPrescales_st row;
St_trigPrescales *tableSet = new St_trigPrescales("trigPrescales",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =    4003006; // run number  ;
    row.idxTrigger	 =          0; // daq trigger id  ;
    row.idxLevel	 =          1; // L1/L2/L3  ;
    row.id	 =          1; // trigger algorithm id  ;
    row.ps	 =          1; // prescale  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
