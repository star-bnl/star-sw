TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar        =           3;       // Outer PressureGPHist534P03if
  row.a[0]	 = 2.67595e+03;
  row.a[1]	 =-7.69432e+02;
  row.a[2]	 = 5.53086e+01;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar        =      3;           // Inner  PressureGPHist534P03if
  row.a[0]	 = 5.28588e+03;// 
  row.a[1]	 =-1.52287e+03;//
  row.a[2]	 = 1.09684e+02;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
