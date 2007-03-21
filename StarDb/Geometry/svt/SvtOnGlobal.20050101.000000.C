TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,-0.00044, 0,1,0.000132, 0.000441,-0.000132,1, 0.221908,0.143028,0.209079,.1,.1,.1,.1,.1,.1,"Inverse Tpc"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
