TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// Positioning of the SSD sectors in the SSD barrel coordinate system
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[4] = {
    {   1,  1.000000, 0., 0.0000, 0., 1., 0., 0.0000, 0., 1.000000, 0., 0., 0., 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,"   Nominal errors - top sector"},
    {   2,  1.000000, 0., 0.0000, 0., 1., 0., 0.0000, 0., 1.000000, 0., 0., 0., 0.1, 0.1, 0.1, 0.1, 0.1, 0.1," Nominal errors - right sector"},
    {   3,  0.999999, 0., 0.0015, 0., 1., 0.,-0.0015, 0., 0.999999, 0., 0., 0., 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,"Nominal errors - bottom sector"},
    {   4,  1.000000, 0., 0.0000, 0., 1., 0., 0.0000, 0., 1.000000, 0., 0., 0., 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,"  Nominal errors - left sector"},
  };
  Int_t n = 4;
  St_Survey *tableSet = new St_Survey("SsdSectorsOnBarrel",n);
  for (Int_t i = 0; i < n; i++) {
    tableSet->AddAt(&row[i].Id, i);
  }
  return (TDataSet *)tableSet;
}
