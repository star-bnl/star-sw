TDataSet *CreateTable() { // Ad hoc accont of saturation in dE/dx
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdEdxCor",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar        =             4;//
  row.min         =           2.0;// 
  row.max         =           5.0;//
  row.a[0]        =    0.00000e+00;//
  row.a[1]        =   -4.48058e-02;// 
  row.a[2]        =    5.19474e-02;// 
  row.a[3]        =   -2.74181e-02;// 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
