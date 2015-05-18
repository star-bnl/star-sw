TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tof/.vpdTotCorr/vpdTotCorr Allocated rows: 1  Used rows: 1  Row size: 48000 bytes
  //  Table: vpdTotCorr_st[0]--> vpdTotCorr_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_vpdTotCorr")) return 0;
  vpdTotCorr_st row;
  Int_t nrows = 38;
  St_vpdTotCorr *tableSet = new St_vpdTotCorr("vpdTotCorr",nrows);
  memset(&row,0,tableSet->GetRowSize());
  for(int i=0;i<nrows;i++) {
    row.tubeId = i+1;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
