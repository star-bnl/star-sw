TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01779,0.00025,-0.01779,0.99984,0.00047,-0.00024,-0.00047,1.00000,-0.1802,-0.0033,-0.1463,0.00000,0.00001,0.00005,0.00007,0.00007,0.00007,"CuCu200RF SVT 0 Pass121D"},
    {1,0.99982,0.01876,0.00010,-0.01876,0.99982,-0.00154,-0.00013,0.00154,1.00000,-0.2238,-0.0081,-0.1806,0.00001,0.00005,0.00006,0.00050,0.00067,0.00061,"CuCu200RF SVT 1 Pass121D"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
