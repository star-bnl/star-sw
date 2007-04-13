TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01802,0.00054,-0.01802,0.99984,0.00027,-0.00053,-0.00028,1.00000,-0.1976,-0.0373,-0.1425,0.00004,0.00001,0.00006,0.00017,0.00005,0.00008,"CuCu200FF SSD 1 Pass126"},
    {1,0.99987,0.01635,0.00039,-0.01635,0.99986,-0.00186,-0.00042,0.00185,1.00000,-0.2520,-0.0347,-0.1781,0.00001,0.00003,0.00007,0.00025,0.00007,0.00009,"CuCu200FF SSD 1 Pass126"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
