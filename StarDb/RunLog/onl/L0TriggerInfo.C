TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.L0TriggerInfo/L0TriggerInfo Allocated rows: 1  Used rows: 1  Row size: 92 bytes
//  Table: L0TriggerInfo_st[0]--> L0TriggerInfo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_L0TriggerInfo")) return 0;
L0TriggerInfo_st row;
St_L0TriggerInfo *tableSet = new St_L0TriggerInfo("L0TriggerInfo",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =          0; // idx_rn  ;
    row.daqTriggerId	 =          0; // id_trigger  ;
    row.offlineTriggerId	 =  0; // offlineBit  ;
    row.psL0	 =          0; // desiredL0PS  ;
 memcpy(&row.name,"epoch\x00",5);// name  
    row.detectorLiveOnBits	 =  0; // ;
    row.detectorLiveOffBits	 =  0; // ;
    row.detectorRequest	 =          0; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
