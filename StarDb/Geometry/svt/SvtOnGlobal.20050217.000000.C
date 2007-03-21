TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.000250,-0.000370,-0.000250,1.000000,0.000092,0.000371,-0.000092,1.000000,
	0.216396,0.134954,0.209593,
	0.000020,0.000020,0.000020,0.000211,0.000206,0.000219,
	"Run6048024 CuCu200FF"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
