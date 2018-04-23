TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52719; // +/- 1.02427e-05 cm/us All: East = -0.967799 +/- 0.245773
  row.laserDriftVelocityWest	 =   5.52719; // +/- 1.02427e-05 cm/us All: West = 0.195145 +/- 0.00182167
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52719 +/- 1.02427e-05
  return (TDataSet *)tableSet;// West = 5.52715 +/- 1.02787e-05 East = 5.53337 +/- 0.000122626
};
