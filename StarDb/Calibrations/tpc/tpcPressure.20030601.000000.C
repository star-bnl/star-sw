TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcPressure Allocated rows: 1
//  Table: tpcPressure_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcPressure")) return 0;
  tpcPressure_st row;
  St_tpcPressure *tableSet = new St_tpcPressure("tpcPressure",2);
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
// ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
