TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); // PressureCGFRunXV02pp200.root
  row.idx = 1;
  row.nrows = 1;
  row.npar = 2; 
  row.a[0] =  2.01167e+01 ;//  
  row.a[1] = -2.90475e+00 ;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
