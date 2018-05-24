TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136004
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53764; // +/- 5.57693e-06 cm/us All: East = 0.162922 +/- 0.00931693
  row.laserDriftVelocityWest	 =   5.53764; // +/- 5.57693e-06 cm/us All: West = 0.169393 +/- 0.000990642
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53764 +/- 5.57693e-06
  return (TDataSet *)tableSet;// West = 5.53764 +/- 5.61754e-06 East = 5.53717 +/- 4.6465e-05
};
