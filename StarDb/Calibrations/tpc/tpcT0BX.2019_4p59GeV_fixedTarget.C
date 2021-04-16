TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.04548, 1768.53,  -0.00000,  0.00081,  -2.0806e-06,   1.2175e-06},
    { 2, "bbc", -0.04715, 1722.92,  -0.00000,  0.00069,  -4.0759e-06,   9.1952e-07},
    { 3, "epd", -0.04355, 1369.02,  -0.00000,  0.00075,   4.7078e-07,   1.1121e-06},
    { 4, "zdc", -0.04197,  303.18,  -0.00002,  0.00488,    3.535e-05,   4.2366e-05},
    {-5, "TAC", -0.04382, 1389.70,  -0.00000,  0.00071,   7.6649e-07,   9.1996e-07} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

