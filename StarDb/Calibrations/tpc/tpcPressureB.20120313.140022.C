TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); // PressureCGFRunXII05pp510P13ia_pp500_production_2012_ReversedFullField
  row.idx = 1;
  row.nrows = 1;
  row.npar = 2; 
  row.a[0] = 18.6004;//   +/-   0.0495751 
  row.a[1] =-2.68783;//   +/-   0.00716397
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
