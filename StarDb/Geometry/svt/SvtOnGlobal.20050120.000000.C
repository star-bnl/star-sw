TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,-0.000130,-0.000360,0.000130,1.000000,0.000122,0.000361,-0.000122,1.000000,
	0.223789,0.148201,0.207473,
	0.000020,0.000020,0.000020,0.000223,0.000222,0.000235,
	"Run6020062 CuCu200RF"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
