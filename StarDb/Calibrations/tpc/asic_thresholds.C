St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tfspars/asic_thresholds Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: asic_thresholds_st[0]--> asic_thresholds_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_asic_thresholds")) return 0;
  asic_thresholds_st row;
  St_asic_thresholds *tableSet = new St_asic_thresholds("asic_thresholds",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.thresh_lo = 1;
  row.thresh_hi = 3;
  row.n_seq_lo  = 3;
  row.n_seq_hi  = 1;
  tableSet->AddAt(&row,0);
 // ----------------- end of code ---------------
  return (St_DataSet *)tableSet;
}
