TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/sst/.sstGainCalibWafer/sstGainCalibWafer Allocated rows: 1  Used rows: 1  Row size: 1280 bytes
  //  Table: sstGainCalibWafer_st[0]--> sstGainCalibWafer_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstGainCalibWafer")) return 0;
  sstGainCalibWafer_st row;
  St_sstGainCalibWafer *tableSet = new St_sstGainCalibWafer("sstGainCalibWafer",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t i = 0; i < 320; i++) row.nGain[i]	 =          1; // ladder[0-19]*16 + wafer[0-15];
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
