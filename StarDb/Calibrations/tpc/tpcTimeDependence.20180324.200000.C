TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------ 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0; 
  tpcCorrection_st row;
  Int_t nrows = 1; 
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeDependence",nrows); 
  memset(&row,0,tableSet->GetRowSize()); // 
  row.idx        = 1;    //
  row.nrows      = nrows;//
/* bin1 = 79460, bin2 = 79756*/
  row.min   =  7.329996e+08;
  row.max   =  7.340688e+08;
  row.npar  =             2;
  row.a[0]  =      5.483058;
  row.a[1]  = -7.454427e-09;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
