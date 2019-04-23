TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcChargeEvent")) return 0;
  tpcChargeEvent_st row;
  Int_t nrows = 1;
  St_tpcChargeEvent *tableSet = new St_tpcChargeEvent("tpcChargeEvent",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
