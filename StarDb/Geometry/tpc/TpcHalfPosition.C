TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 2;// east = 0, west = 1
  St_Survey *tableSet = new St_Survey("TpcHalfPosition",n);
  for (Int_t half = 0; half <= 1; half++) {
    row.Id = half;
    tableSet->AddAt(&row.Id);
  }
  return (TDataSet *)tableSet;
}
