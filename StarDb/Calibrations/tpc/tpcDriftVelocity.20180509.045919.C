TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 129002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55741; // +/- 6.57639e-06 cm/us All: East = -0.230304 +/- 0.00555216
  row.laserDriftVelocityWest	 =   5.55741; // +/- 6.57639e-06 cm/us All: West = 0.283693 +/- 0.00118974
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55741 +/- 6.57639e-06
  return (TDataSet *)tableSet;// West = 5.55729 +/- 6.74794e-06 East = 5.55954 +/- 2.9352e-05
};
