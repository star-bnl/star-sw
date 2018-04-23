TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52801; // +/- 8.35568e-06 cm/us All: East = -0.710669 +/- 0.00817097
  row.laserDriftVelocityWest	 =   5.52801; // +/- 8.35568e-06 cm/us All: West = 0.192531 +/- 0.00153888
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52801 +/- 8.35568e-06
  return (TDataSet *)tableSet;// West = 5.52784 +/- 8.49638e-06 East = 5.53282 +/- 4.61042e-05
};
