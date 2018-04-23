TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53271; // +/- 1.70638e-05 cm/us All: East = -0.580274 +/- 0.00700101
  row.laserDriftVelocityWest	 =   5.53271; // +/- 1.70638e-05 cm/us All: West = 0.290179 +/- 0.00348283
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53271 +/- 1.70638e-05
  return (TDataSet *)tableSet;// West = 5.53179 +/- 1.89537e-05 East = 5.53662 +/- 3.92005e-05
};
