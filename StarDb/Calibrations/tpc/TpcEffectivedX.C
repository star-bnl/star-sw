TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcEffectivedX")) return 0;
  TpcEffectivedX_st row;
  St_TpcEffectivedX *tableSet = new St_TpcEffectivedX("TpcEffectivedX",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.scaleInner = 1;
  row.scaleOuter = 1;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
