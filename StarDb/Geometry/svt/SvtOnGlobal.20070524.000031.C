TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	0.999895,0.014490,-0.000826,-0.014490,0.999895,0.000641,0.000834,-0.000629,1.000000,
	-0.013733,0.145502,0.053507,
	0.000020,0.000000,0.000020,0.000273,0.000264,0.000037,
	"Run8159044 AuAu200RF"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
