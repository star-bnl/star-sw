TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar        =          2;       // Outer PressureCGPHist716Fast
  row.a[0]	 = 2.26174e+01;
  row.a[1]	 =-3.26913e+00;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar        =      2;           // Inner 
  row.a[0]	 = 3.21909e+01;// 
  row.a[1]	 =-4.65444e+00;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
