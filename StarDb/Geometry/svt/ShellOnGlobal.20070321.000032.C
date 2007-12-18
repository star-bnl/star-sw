TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99999,0.00506,0.00022,-0.00506,0.99999,-0.00020,-0.00022,0.00020,1.00000, 0.0040, 0.0577, 0.0331,0.00001,0.00002,0.00002,0.00020,0.00027,0.00003,"SVT 0 Pass213 FFA"},
    {1,0.99998,0.00616,0.00096,-0.00616,0.99998,-0.00089,-0.00097,0.00088,1.00000,-0.0088, 0.0427, 0.0034,0.00002,0.00002,0.00002,0.00023,0.00003,0.00024,"SVT 1 Pass213 FFA"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
