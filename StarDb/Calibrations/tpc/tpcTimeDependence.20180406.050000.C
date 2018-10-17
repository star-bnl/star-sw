TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------ 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0; 
  tpcCorrection_st row;
  Int_t nrows = 1; 
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcTimeDependence",nrows); 
  memset(&row,0,tableSet->GetRowSize()); // 
  row.idx        = 1;    //
  row.nrows      = nrows;//
/* bin1 = 79757, bin2 = 79844*/
  row.min   =  7.340688e+08;
  row.max   =  7.343856e+08;
  row.npar  =             2;
  row.a[0]  =      17.71233;
  row.a[1]  = -2.412081e-08;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
