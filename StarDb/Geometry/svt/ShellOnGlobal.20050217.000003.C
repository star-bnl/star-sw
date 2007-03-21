TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01797,0.00013,-0.01797,0.99984,0.00018,-0.00013,-0.00018,1.00000,-0.2046,-0.0380,-0.1463,0.00003,0.00001,0.00005,0.00006,0.00006,0.00006,"CuCu200FF SVT 0 Pass121D"},
    {1,0.99985,0.01736,0.00011,-0.01736,0.99985,-0.00192,-0.00014,0.00192,1.00000,-0.2497,-0.0429,-0.1748,0.00001,0.00005,0.00006,0.00008,0.00065,0.00060,"CuCu200FF SVT 1 Pass121D"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
