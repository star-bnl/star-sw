TDataSet *CreateTable() { 
  // strip coordinate (x,y,0) => module coordinate (y,0,x)
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st rows[8] = {
    //m                        u   v  w  
    {0, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 West"},
    {1, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 West"},
    {2, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 East"},
    {3, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 East"},
    {4, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 West"},
    {5, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 West"},
    {6, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 East"},
    {7, 0, 1,0, 0,0,-1, 1,0,0,-10,-10,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 4 East"}};
  Int_t noModules = 8;// 
  St_Survey *tableSet = new St_Survey("GmtOnModule",noModules);
  TString Rot;
  for (Int_t m = 0; m < noModules; m++) {
    tableSet->AddAt(&rows[m].Id);
  }
  return (TDataSet *) tableSet;
}
