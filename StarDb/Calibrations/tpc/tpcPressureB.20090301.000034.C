TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx        =            1; //  PressureCGFRunIX34P09if
  row.nrows      =            1; //
  row.npar       =            2; //
  row.a[0]       =  2.04965e+01; //
  row.a[1]       = -2.96241e+00; //
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
