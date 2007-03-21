TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.000590,-0.000020,-0.000590,1.000000,0.000252,0.000021,-0.000252,1.000000,
	0.227180,0.154368,0.198027,
	0.000010,0.000020,0.000020,0.000394,0.000346,0.000380,
	"Run6020062 CuCu200RF"};
  St_Survey *tableSet = new St_Survey("SsdOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
