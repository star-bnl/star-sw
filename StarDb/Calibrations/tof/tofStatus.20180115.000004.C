TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tof/.tofStatus/tofStatus Allocated rows: 1  Used rows: 1  Row size: 48000 bytes
  //  Table: tofStatus_st[0]--> tofStatus_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tofStatus")) return 0;
  tofStatus_st row;
  St_tofStatus *tableSet = new St_tofStatus("tofStatus",1);
  Int_t mNTray = 120;
  Int_t mNModule = 32;
  Int_t mNCell = 6;
  Int_t mNChanMax = 24000;  /// A large number for total channels
  //
  memset(&row,0,tableSet->GetRowSize());
  for(int i=0;i<mNChanMax;i++) {
    int trayId = i/(mNModule*mNCell) + 1;
    int moduleId = (i%(mNModule*mNCell))/mNCell + 1;
    int cellId = i%mNCell + 1;
    if(trayId<=0||trayId>mNTray||moduleId<=0||moduleId>mNModule||cellId<=0||cellId>mNCell) continue;
    row.status[i] = 1;
  }
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
