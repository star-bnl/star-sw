TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99999,0.00512,0.00029,-0.00512,0.99999,-0.00055,-0.00030,0.00055,1.00000,-0.0024, 0.0548, 0.0339,0.00003,0.00001,0.00004,0.00005,0.00049,0.00039,"SVT 0 Pass213 RFE"},
    {1,0.99997,0.00743,0.00075,-0.00743,0.99997,-0.00044,-0.00075,0.00044,1.00000,-0.0173, 0.0382, 0.0023,0.00003,0.00004,0.00004,0.00043,0.00062,0.00046,"SVT 1 Pass213 RFE"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
