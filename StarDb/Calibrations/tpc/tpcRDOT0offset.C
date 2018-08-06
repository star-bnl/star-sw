TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcRDOT0offset")) return 0;
  tpcRDOT0offset_st row;
  St_tpcRDOT0offset *tableSet = new St_tpcRDOT0offset("tpcRDOT0offset",1);
  memset(&row, 0, tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
