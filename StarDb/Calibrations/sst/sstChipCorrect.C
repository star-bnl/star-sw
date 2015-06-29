TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/sst/.sstChipCorrect/sstChipCorrect Allocated rows: 1  Used rows: 1  Row size: 15360 bytes
  //  Table: sstChipCorrect_st[0]--> sstChipCorrect_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstChipCorrect")) return 0;
  sstChipCorrect_st row;
  St_sstChipCorrect *tableSet = new St_sstChipCorrect("sstChipCorrect",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
