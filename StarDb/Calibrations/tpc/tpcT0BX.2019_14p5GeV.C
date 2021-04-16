TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
    { 1, "vpd", -0.02329, 1456.05,  0.00001,  0.00035,  -1.7868e-05,   1.2174e-06},
    { 2, "bbc", -0.02323, 1823.65,  0.00001,  0.00034,  -1.2904e-05,   1.0366e-06},
    { 3, "epd", -0.02305, 1331.69,  0.00001,  0.00035,  -1.5843e-05,   1.1779e-06},
    { 4, "zdc", -0.02360,  262.52,  0.00001,  0.00040,  -4.7006e-05,   5.1989e-06},
    {-5, "TAC", -0.02303, 1507.04,  0.00001,  0.00034,  -1.4174e-05,   1.0808e-06} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

