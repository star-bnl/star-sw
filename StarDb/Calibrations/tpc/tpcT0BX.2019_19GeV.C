TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.01950, 1448.76,  0.00001,  0.00034,  -1.6359e-05,   1.4268e-06},
    { 2, "bbc", -0.01948, 1712.62,  0.00001,  0.00032,  -6.2716e-06,   9.9846e-07},
    { 3, "epd", -0.01945, 1420.56,  0.00001,  0.00032,  -6.0746e-06,   9.9589e-07},
    { 4, "zdc", -0.01980,  276.76,  0.00002,  0.00035,  -5.4705e-05,   5.5086e-06},
    {-5, "TAC", -0.01945, 1576.40,  0.00001,  0.00032,  -7.7191e-06,   1.0476e-06} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

