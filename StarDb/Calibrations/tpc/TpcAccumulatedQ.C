TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrect192")) return 0;
  tpcCorrect192_st row;
  St_tpcCorrect192 *tableSet = new St_tpcCorrect192("TpcAccumulatedQ",192);
  memset(&row,0,tableSet->GetRowSize()); 
  for (Int_t i = 0; i < 192; i++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
