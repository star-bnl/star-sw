TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx = 1;
  row.nrows = 1;
  row.npar = 2; 
  row.a[0] = 2.08917e+01;//   +/-7.08025e-04
  row.a[1] =-3.01882e+00;//   +/-1.02309e-04
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
