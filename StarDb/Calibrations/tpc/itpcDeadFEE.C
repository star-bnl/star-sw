TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_itpcDeadFEE")) return 0;
  itpcDeadFEE_st row;
  St_itpcDeadFEE *tableSet = new St_itpcDeadFEE("itpcDeadFEE",1);
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
