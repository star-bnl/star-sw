TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",10);
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
