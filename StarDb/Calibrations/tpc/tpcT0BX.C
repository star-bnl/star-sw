TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    {-1, "vpd", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {-2, "bbc", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {-3, "epd", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {-4, "zdc", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {-5, "TAC", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

