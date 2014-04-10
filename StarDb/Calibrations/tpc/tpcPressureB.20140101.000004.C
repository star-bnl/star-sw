TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); // PressureCGFRunXIII04pp500
  row.idx = 1;
  row.nrows = 1;
  row.npar = 2; 
  row.a[0] =   2.14771e+01;//   +/- 6.70274e-04 
  row.a[1] =  -3.10259e+00;//   +/- 9.68248e-05
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
