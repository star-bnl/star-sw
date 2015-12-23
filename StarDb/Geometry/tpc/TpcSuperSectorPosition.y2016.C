TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 24;
  St_Survey *tableSet = new St_Survey("TpcSuperSectorPosition",n);
  for (Int_t sec = 1; sec <= n; sec++) {
    row.Id = sec;
    tableSet->AddAt(&row.Id);
  }
  return (TDataSet *)tableSet;
}
