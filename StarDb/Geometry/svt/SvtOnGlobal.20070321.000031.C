TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	0.999896,0.014440,-0.000586,-0.014440,0.999896,0.000311,0.000590,-0.000303,1.000000,
	0.001643,0.140310,0.054745,
	0.000020,0.000000,0.000020,0.000222,0.000214,0.000187,
	"Run8120052 AuAu200FF"};
  St_Survey *tableSet = new St_Survey("SvtOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
