TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  //                                -alpha      alpha
  Survey_st rowE = {0, 1,0,0, 0,1,-0.00000, 0,0.00000,1, 0,0,0, 0,0,0,0,0,0,"Ideal"};
  Survey_st rowW = {0, 1,0,0, 0,1,-0.00000, 0,0.00000,1, 0,0,0, 0,0,0,0,0,0,"Ideal"};
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
