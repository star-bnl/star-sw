TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55188; // +/- 0.000137885 cm/us All: East = 3.33043 +/- 21.0499
  row.laserDriftVelocityWest	 =   5.55188; // +/- 0.000137885 cm/us All: West = 0.176512 +/- 0.22177
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55188 +/- 0.000137885
  return (TDataSet *)tableSet;// West = 5.55188 +/- 0.000137886 East = 5.45781 +/- 0.0438043
};
