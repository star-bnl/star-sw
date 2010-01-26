TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcMethaneIn",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx        =           1; // percentMethaneInPCGFRunIX47
  row.nrows      =           1; //
  row.npar       =           2; //
  row.a[0]       = 6.93207e-02; // 
  row.a[1]       =-6.75124e-03; //
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
