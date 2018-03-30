TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51741; // +/- 8.15194e-06 cm/us All: East = 4.55929 +/- 0.00687539
  row.laserDriftVelocityWest	 =   5.51741; // +/- 8.15194e-06 cm/us All: West = 4.44708 +/- 0.00150221
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51741 +/- 8.15194e-06
  return (TDataSet *)tableSet;// West = 5.51743 +/- 8.35164e-06 East = 5.51683 +/- 3.75023e-05
};
