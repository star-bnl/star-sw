TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcPressureB",1);
  memset(&row,0,tableSet->GetRowSize()); // PressureCGFRunXIII04pp500
  row.idx = 1;
  row.nrows = 1;
  row.npar = 2; 
  row.a[0] =  1.53590e+01 +3.25444e+00;// +/-   1.39785e-03
  row.a[1] = -2.21808e+00 -4.70033e-01;// +/-   2.01857e-04
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
