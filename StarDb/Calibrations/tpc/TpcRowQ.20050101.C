TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",45);
  memset(&row,0,tableSet->GetRowSize()); 
  
  for (Int_t i = 0; i < 45; i++) {
    row.idx  = i + 1;
    row.a[0] = 0.023719;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
