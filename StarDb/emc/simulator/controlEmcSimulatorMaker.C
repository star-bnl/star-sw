TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// controlEmcSimulatorMaker Allocated rows: 1  Used rows: 1  Row size: 10 bytes
//  Table: controlEmcSimulatorMaker_st[0]--> controlEmcSimulatorMaker_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_controlEmcSimulatorMaker")) return 0;
controlEmcSimulatorMaker_st row;
St_controlEmcSimulatorMaker *tableSet = new St_controlEmcSimulatorMaker("controlEmcSimulatorMaker",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.bemc	 =          2; // switch for BEMC ;
    row.eemc	 =          0; // switch for EEMC ;
    row.hist	 =          1; // switch for hist ;
    row.debug	 =          0; // 0=debug of; >1=debug on ;
    row.messLimit	 =        200; // limit for warning message ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
