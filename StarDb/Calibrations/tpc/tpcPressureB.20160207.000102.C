TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx = 1;
  row.nrows = 2;
  row.npar = 2; 
  row.a[0] = 1.89644e+01-1.35208e-01+2.79633e+00;// PressureCGFRunXVIAuAu200p102
  row.a[1] =-2.73952e+00+1.88374e-02-4.03835e-01;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx = 2;
  row.nrows = 2;
  row.npar = 2; 
  row.a[0] = 1.89644e+01-8.55752e+00+4.34649e+00;// PressureCGFRunXVIAuAu200p102
  row.a[1] =-2.73952e+00+1.23578e+00-6.28382e-01;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
