TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st rowE = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Survey_st rowW = {0, 1,0,0, 0,1,-5e-4, 0,5e-4,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Ideal"};
  Int_t n = 24;
  St_Survey *tableSet = new St_Survey("TpcInnerSectorPositionB",n);
  for (Int_t sec = 1; sec <= n; sec++) {
    row.Id = sec;
    if (sec >= 12)
      tableSet->AddAt(&rowE.Id);
    else 
      tableSet->AddAt(&rowW.Id);
  }
  return (TDataSet *)tableSet;
}
