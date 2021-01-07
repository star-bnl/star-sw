TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 48;
  St_Survey *tableSet = new St_Survey("TpcInnerSectorPositionB",n);
  for (Int_t i = 0; i < n; i++) {
    row.Id = i%24 + 1;
    tableSet->AddAt(&row.Id);
  }
  return (TDataSet *)tableSet;
}
