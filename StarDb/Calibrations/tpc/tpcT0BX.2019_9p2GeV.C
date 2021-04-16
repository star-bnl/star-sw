TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.01869, 1818.67,  0.00001,  0.00047,  -1.6116e-05,   8.3786e-07},
    { 2, "bbc", -0.01863, 1690.90,  0.00001,  0.00049,  -1.5385e-05,   8.4715e-07},
    { 3, "epd", -0.01872, 1506.99,  0.00001,  0.00050,  -1.5076e-05,   8.7881e-07},
    { 4, "zdc", -0.02232,  303.81,  0.00001,  0.00085,  -4.9238e-05,   7.5253e-06},
    {-5, "TAC", -0.01848, 1672.43,  0.00001,  0.00049,  -1.4139e-05,   8.1884e-07} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detdetId);
  return (TDataSet *)tableSet;
}

