TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.02418, 1866.96,  0.00000,  0.00046,  -1.5482e-05,   8.1083e-07},
    { 2, "bbc", -0.02481, 1743.40,  0.00000,  0.00047,  -1.6182e-05,   8.0909e-07},
    { 3, "epd", -0.02499, 1515.32,  0.00000,  0.00047,  -1.6192e-05,   8.3259e-07},
    { 4, "zdc", -0.02740,  322.15,  0.00002,  0.00151,  -2.5805e-05,   1.2931e-05},
    {-5, "TAC", -0.02454, 1673.90,  0.00000,  0.00047,  -1.5285e-05,   7.8793e-07} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

