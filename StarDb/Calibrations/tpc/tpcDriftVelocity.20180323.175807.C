TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53723; // +/- 1.07437e-05 cm/us All: East = 0.274326 +/- 0.00763553
  row.laserDriftVelocityWest	 =   5.53723; // +/- 1.07437e-05 cm/us All: West = 0.902643 +/- 0.00196901
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53723 +/- 1.07437e-05
  return (TDataSet *)tableSet;// West = 5.53699 +/- 1.11443e-05 East = 5.54039 +/- 4.0433e-05
};
