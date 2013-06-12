TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcSlewing")) return 0;
  tpcSlewing_st row;
  St_tpcSlewing *tableSet = new St_tpcSlewing("tpcSlewing",2);
  memset(&row,0,tableSet->GetRowSize());
  row.type	 =         -1; // ;
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
