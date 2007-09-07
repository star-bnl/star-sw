TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row[2] = {
    {0,0.99999,0.00517,0.00019,-0.00517,0.99999,-0.00014,-0.00019,0.00014,1.00000,-0.0029, 0.0590, 0.0348,0.00002,0.00003,0.00003,0.00033,0.00044,0.00035,"SVT 0 Pass207 FFC"},
    {1,0.99998,0.00627,0.00090,-0.00627,0.99998,-0.00085,-0.00091,0.00084,1.00000,-0.0012, 0.0423, 0.0046,0.00003,0.00003,0.00002,0.00038,0.00052,0.00039,"SVT 1 Pass207 FFC"}
  };
  St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
  for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
  return (TDataSet *)tableSet;
}
