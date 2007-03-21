TDataSet *CreateTable() {
	if (!gROOT->GetClass("St_Survey")) return 0;
	Survey_st row[2] = {
	{0,0.99984,0.01759,-0.00031,-0.01759,0.99984,0.00046,0.00032,-0.00045,1.00000,-0.1892,-0.0221,-0.1463,0.00002,0.00003,0.00013,0.00099,0.00018,0.00099,"Pass103/065BPass37E (+x South) Shell"},
	{1,0.99984,0.01789,-0.00051,-0.01789,0.99984,-0.00191,0.00048,0.00192,1.00000,-0.2340,-0.0206,-0.1665,0.00003,0.00004,0.00017,0.00137,0.00018,0.00020,"Pass103/065BPass37E (-x North) Shell"}
	};
	St_Survey *tableSet = new St_Survey("ShellOnGlobal",2);
	for (Int_t i = 0; i < 2; i++) tableSet->AddAt(&row[i].Id, i);
	return (TDataSet *)tableSet;
	}
