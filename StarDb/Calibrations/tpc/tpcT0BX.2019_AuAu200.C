TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcT0BX")) return 0;
  tpcT0BX_st row[5] = {
     { 1, "vpd", -0.00380, 2190.45,  0.00000,  0.00036,    -1.73e-05,   1.4957e-06},
     { 2, "bbc", -0.00383, 1618.44,  0.00000,  0.00034,  -1.2742e-05,    1.126e-06},
     { 3, "epd", -0.00373, 2049.21,  0.00000,  0.00038,  -1.6007e-05,   1.4364e-06},
     { 4, "zdc", -0.00380,  359.93,  0.00001,  0.00033,   -5.379e-05,   4.1424e-06},
     {-5, "TAC", -0.00374, 2218.49,  0.00000,  0.00037,  -1.4185e-05,   1.3058e-06} 
  };
  St_tpcT0BX *tableSet = new St_tpcT0BX("tpcT0BX",5);
  for (Int_t i = 0; i < 5; i++) tableSet->AddAt(&row[i].detId);
  return (TDataSet *)tableSet;
}

