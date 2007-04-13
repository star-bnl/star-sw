TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99983,0.01822,0.00056,-0.01822,0.99983,0.00015,-0.00056,-0.00016,1.00000,-0.1875,-0.0223,-0.1417,0.00005,0.00001,0.00007,0.00008,0.00078,0.00008,"SVT 0 Pass126Cu62"},
    {1,0.99985,0.01751,0.00049,-0.01751,0.99985,-0.00148,-0.00052,0.00147,1.00000,-0.2126,-0.0304,-0.1668,0.00001,0.00007,0.00009,0.00012,0.00012,0.00090,"SVT 1 Pass126Cu62"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
