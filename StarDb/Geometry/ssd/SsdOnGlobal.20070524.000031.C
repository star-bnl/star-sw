TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.001000,-0.000530,-0.001000,1.000000,0.000932,0.000531,-0.000932,1.000000,
	0.184638,0.210131,0.218868,
	0.000000,0.000020,0.000000,0.000069,0.000075,0.000099,
	"Run8159044 AuAu200RF"};
  St_Survey *tableSet = new St_Survey("SsdOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
