TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	0.999892,0.014690,-0.000576,-0.014690,0.999892,0.000401,0.000581,-0.000393,1.000000,
	0.000377,0.141680,0.056701,
	0.000020,0.000020,0.000020,0.000229,0.000222,0.000194,
	"Run8080022 AuAu200FFE"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
