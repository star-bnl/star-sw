TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01800,0.00024,-0.01800,0.99984,0.00039,-0.00023,-0.00039,1.00000,-0.1795, 0.0032,-0.1460,0.00000,0.00000,0.00008,0.00011,0.00011,0.00010,"CuCu200RF SVT 0 Pass126"},
    {1,0.99983,0.01840,0.00033,-0.01840,0.99983,-0.00175,-0.00036,0.00174,1.00000,-0.2235,-0.0015,-0.1860,0.00001,0.00001,0.00009,0.00072,0.00012,0.00011,"CuCu200RF SVT 1 Pass126"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
