TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.03698, 1293.47,  0.00000,  0.00039,  -1.8331e-05,   8.8685e-07},
    { 2, "bbc", -0.03662, 1439.95,  0.00000,  0.00038,  -1.7001e-05,    8.386e-07},
    { 3, "epd", -0.03658, 1274.89,  0.00000,  0.00039,  -1.7426e-05,   8.8167e-07},
    { 4, "zdc", -0.03843,  291.95,  0.00002,  0.00075,  -5.1504e-05,   6.8779e-06},
    {-5, "TAC", -0.03654, 1445.56,  0.00000,  0.00038,  -1.6556e-05,   8.2322e-07} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

