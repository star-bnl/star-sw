TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  if (!gROOT->GetClass("St_tpcSectorPosition")) return 0;
  tpcSectorPosition_st row;
  St_tpcSectorPosition *tableSet = new St_tpcSectorPosition("tpcSectorPosition",1);
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

