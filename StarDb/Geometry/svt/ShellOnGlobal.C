TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// These are simple matrices that bring the SVT Clamshells
// into their final position in the STAR Global Coordinate system.
// 
//
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
      {0,  0.999850,  0.01733,  0.000019, -0.01733,   0.999850,  -0.00071, -0.000019, 0.00071,  0.999999, -0.2105, -0.0160, -0.1244, 0.002, 0.002, 0.002, 0.0500, 0.0500, 0.0500,"0 is the x (South) Shell"},
      {1,  0.999850,  0.01717,  -0.00089, -0.01717,   0.999850,  -0.000077, 0.00089, 0.000077,  0.999999, -0.2587, -0.0316, -0.1533, 0.002, 0.002, 0.002, 0.0500, 0.0500, 0.0500,"1 is the -x (North) Shell"},
  };
  Int_t n = 2;
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",n);
  for (Int_t i = 0; i < n; i++) {
    tableSet->AddAt(&row[i].Id, i);
  }
  return (TDataSet *)tableSet;
}


