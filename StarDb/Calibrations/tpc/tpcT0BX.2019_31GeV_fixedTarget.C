TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.07243, 2027.38,  -0.00000,  0.00091,   -3.275e-05,   3.2365e-06},
    { 2, "bbc", -0.07254, 1280.74,  -0.00000,  0.00085,  -2.4409e-05,   1.8724e-06},
    { 3, "epd", -0.07185, 1357.25,  -0.00000,  0.00087,  -2.4457e-05,   2.6002e-06},
    { 4, "zdc", -0.07069,  236.12,  -0.00000,  0.00088,  -9.1613e-05,   1.1882e-05},
    {-5, "TAC", -0.07170, 1487.22,  -0.00000,  0.00086,  -2.3019e-05,   2.4419e-06} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

