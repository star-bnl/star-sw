TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = 
	{0,
	1.000000,0.000970,-0.000340,-0.000970,1.000000,0.000492,0.000341,-0.000492,1.000000,
	0.201665,0.207184,0.211269,
	0.000000,0.000000,0.000020,0.000057,0.000476,0.000081,
	"Run8080022 AuAu200FFC"};
  St_Survey *tableSet = new St_Survey("SsdOnGlobal",1);
  tableSet->AddAt(&row.Id, 0);
  return (TDataSet *)tableSet;
}
