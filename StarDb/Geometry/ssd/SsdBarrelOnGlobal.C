TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// Positioning of the SSD barrel in the STAR global coordinate system
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[1] = {
    { 0,  0.999838, 0.018000, 0.000000, -0.018000, 0.999838, 0.000000, 0.000000, 0.000000, 1.000000, -0.399300, 0.021700, -0.090000, 0.100000, 0.100000, 0.100000, 0.100000, 0.100000, 0.100000,"     Deduced from the tracking"},
  };
  Int_t n = 1;
  St_Survey *tableSet = new St_Survey("SsdBarrelOnGlobal",n);
  for (Int_t i = 0; i < n; i++) {
    tableSet->AddAt(&row[i].Id, i);
  }
  return (TDataSet *)tableSet;
}
