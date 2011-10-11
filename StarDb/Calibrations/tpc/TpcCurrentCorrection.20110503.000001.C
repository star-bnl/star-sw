TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
	//From /star/u/yiguo/bnl/workdir/dEdx/hist/RunXI58AuAu200RFF/AvCurrentGFRunXI58AuAu200RFF.root
  memset(&row,0,tableSet->GetRowSize()); //OutSector 
  row.idx = 1;
	row.nrows = nrows;
	row.npar = 102;
	row.a[0] = 4.10794e-03;
	row.a[1] = -7.33870e-02;
	row.min = 0.;
	row.max = 0.4;
	tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); //InnerSector
  row.idx = 2;
  row.nrows = nrows;
  row.npar = 102;
  row.a[0] = 1.51260e-02;
  row.a[1] = -1.01391e-01;
	row.min = 0.;
	row.max = 0.35;	
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

