TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrection",2);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =      0;  // 
  row.min        =   20.0;
  row.max        =  210.0;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =      0;  // 
  row.min        =   20.0;
  row.max        =  210.0;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
