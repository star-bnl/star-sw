TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcPadrowT0")) return 0;
  tpcPadrowT0_st row;
  St_tpcPadrowT0 *tableSet = new St_tpcPadrowT0("tpcPadrowT0",24);
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t s = 1; s <= 24; s++) tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
