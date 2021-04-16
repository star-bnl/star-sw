TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.05789, 1531.56,  0.00001,  0.00042,  -1.8091e-05,   7.5933e-07},
    { 2, "bbc", -0.05775, 1733.87,  0.00001,  0.00040,  -1.7602e-05,   6.6238e-07},
    { 3, "epd", -0.05758, 1508.24,  0.00001,  0.00040,  -1.8213e-05,    6.647e-07},
    { 4, "zdc", -0.06079,  337.02,  0.00000,  0.00127,  -4.1391e-05,   9.4056e-06},
    {-5, "TAC", -0.05758, 1669.02,  0.00001,  0.00040,    -1.78e-05,   6.5519e-07} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

