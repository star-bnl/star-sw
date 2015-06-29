TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // bfc/.make/db/.const/StarDb/Calibrations/sst/.sstMaskChip/sstMaskChip Allocated rows: 1  Used rows: 1  Row size: 7680 bytes
  //  Table: sstMaskChip_st[0]--> sstMaskChip_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstMaskChip")) return 0;
  sstMaskChip_st row;
  St_sstMaskChip *tableSet = new St_sstMaskChip("sstMaskChip",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
