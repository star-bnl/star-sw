TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01776,-0.00083,-0.01776,0.99984,0.00040,0.00084,-0.00038,1.00000,-0.1950,-0.0199,-0.1475,0.00003,0.00003,0.00013,0.00019,0.00022,0.00025,"Pass37E (+x South) Shell"},
    {1,0.99983,0.01839,-0.00030,-0.01839,0.99983,-0.00148,0.00027,0.00149,1.00000,-0.2271,-0.0276,-0.1681,0.00003,0.00003,0.00016,0.00021,0.00023,0.00030,"Pass37E (-x North) Shell"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
