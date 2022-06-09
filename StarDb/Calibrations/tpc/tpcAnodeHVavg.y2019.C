TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAnodeHVavg")) return 0;
  St_tpcAnodeHVavg *tableSet = new St_tpcAnodeHVavg("tpcAnodeHVavg",19*24);
  tpcAnodeHVavg_st row;
  memset(&row, 0, tableSet->GetRowSize());
  for (UShort_t sector = 1; sector <= 24; sector++) 
    for (UShort_t socket = 1; socket <= 19; socket++) {
      row.sector = sector;
      row.socket = socket;
      if (socket <= 8 || socket == 17) row.voltage = 1100;
      else                             row.voltage = 1390;
      tableSet->AddAt(&row);
    }
  return (TDataSet *)tableSet;
}
