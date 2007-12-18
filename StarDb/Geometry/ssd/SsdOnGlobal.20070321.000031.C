TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.000870,-0.000390,-0.000870,1.000000,0.000492,0.000391,-0.000492,1.000000,
	0.200551,0.206432,0.212608,
	0.000000,0.000000,0.000020,0.000055,0.000052,0.000088,
	"Run8120052 AuAu200FF"};
  St_Survey *tableSet = new St_Survey("SsdOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
