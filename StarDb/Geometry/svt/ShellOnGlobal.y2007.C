TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// These are simple matrices that bring the SVT Clamshells
// into their final position in the STAR Global Coordinate system.
// This is ideal Geometry
//
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,  1, 0, 0,  0, 1, 0,  0, 0, 1,  0, 0, 0,  0,0,0,0,0,0, "0 is the x (South) Shell"},
    {1,  1, 0, 0,  0, 1, 0,  0, 0, 1,  0, 0, 0,  0,0,0,0,0,0, "1 is the -x (North) Shell"},
  };
  Int_t n = 2;
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",n);
  for (Int_t i = 0; i < n; i++) {
    tableSet->AddAt(&row[i].Id, i);
  }
  return (TDataSet *)tableSet;
}


