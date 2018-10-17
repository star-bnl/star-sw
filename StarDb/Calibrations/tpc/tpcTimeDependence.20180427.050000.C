TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------ 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0; 
  tpcCorrection_st row;
  Int_t nrows = 1; 
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeDependence",nrows); 
  memset(&row,0,tableSet->GetRowSize()); // 
  row.idx        = 1;    //
  row.nrows      = nrows;//
/* bin1 = 80261, bin2 = 80334*/
  row.min   =  7.358832e+08;
  row.max   =  7.361496e+08;
  row.npar  =             2;
  row.a[0]  =     -22.32034;
  row.a[1]  =  3.032308e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
