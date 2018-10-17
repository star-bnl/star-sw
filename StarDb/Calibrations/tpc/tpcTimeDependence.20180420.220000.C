TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------ 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0; 
  tpcCorrection_st row;
  Int_t nrows = 1; 
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeDependence",nrows); 
  memset(&row,0,tableSet->GetRowSize()); // 
  row.idx        = 1;    //
  row.nrows      = nrows;//
/* bin1 = 80110, bin2 = 80260*/
  row.min   =  7.353396e+08;
  row.max   =  7.358832e+08;
  row.npar  =             2;
  row.a[0]  =      -7.19426;
  row.a[1]  =  9.772769e-09;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
