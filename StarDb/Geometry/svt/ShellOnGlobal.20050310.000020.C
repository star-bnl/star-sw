TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99984,0.01788,0.00033,-0.01788,0.99984,0.00004,-0.00033,-0.00005,1.00000,-0.1866,-0.0227,-0.1430,0.00001,0.00001,0.00008,0.00069,0.00089,0.00009,"SVT 0 Pass126 Cu62D"},
    {1,0.99983,0.01819,0.00047,-0.01819,0.99983,-0.00151,-0.00050,0.00150,1.00000,-0.2161,-0.0294,-0.1674,0.00001,0.00001,0.00011,0.00013,0.00122,0.00013,"SVT 1 Pass126 Cu62D"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
