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
    row.bemc	   =          2; // switch for BEMC ;
    row.eemc	   =          0; // switch for EEMC ;
    row.hist	   =          2; // switch for hist ;
    row.debug	   =          0; // 0=debug of; >1=debug on ;
    row.messLimit  =        200; // limit for warning message ;
// 13-may-2002 =====================================================================
/* 0=no DB; 1 = no noise added; 2 = with noise(pedestal) 
   for embedding must be 1.
*/
    row.keyDB[0] = 0;
    row.keyDB[1] = 0;
    row.keyDB[2] = 0;
    row.keyDB[3] = 0;

    /* time stamp */
    row.dateDB = 20020112;
    row.timeDB = 000000;

    for(Int_t i=4; i<8; i++) row.keyDB[i] = 0; 
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
