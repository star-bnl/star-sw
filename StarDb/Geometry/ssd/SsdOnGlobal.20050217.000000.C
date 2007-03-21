TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.000230,-0.000020,-0.000230,1.000000,0.000002,0.000021,-0.000002,1.000000,
	0.213237,0.132498,0.198764,
	0.000010,0.000010,0.000010,0.000345,0.000323,0.000335,
	"Run6048024 CuCu200FF"};
  St_Survey *tableSet = new St_Survey("SsdOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
