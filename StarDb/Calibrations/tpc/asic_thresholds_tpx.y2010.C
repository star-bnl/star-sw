TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_asic_thresholds_tpx")) return 0;
  St_asic_thresholds_tpx *tableSet = new St_asic_thresholds_tpx("asic_thresholds_tpx",24);
  asic_thresholds_tpx_st row;
  row.thresh_lo = 3; // (threshold) min Adc 
  row.thresh_hi = 0; // not used 
  row.n_seq_lo  = 2; // (counts) min no of time backets 
  row.n_seq_hi  = 0; // not used 
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
