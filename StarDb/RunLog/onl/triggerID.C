#include "tables/St_triggerID_Table.h"

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/RunLog/onl/.triggerID/triggerID Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: triggerID_st[0]--> triggerID_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_triggerID")) return 0;
triggerID_st row;
St_triggerID *tableSet = new St_triggerID("triggerID",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =          0; // run number  ;
    row.idxTrg	 =          0; // ;
    row.daqTrgId	 =          0; // ;
    row.offlineTrgId	 =          0; // bit in L3_Summary(0);  ;
    row.trgNameVersion	 =          0; // identifier bit to offline  ;
    row.trgVersion	 =          0; // ;
    row.threashVersion	 =          0; // ;
    row.psVersion	 =          0; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
