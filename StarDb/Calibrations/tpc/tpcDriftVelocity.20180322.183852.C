TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53526; // +/- 1.44459e-05 cm/us All: East = -0.243265 +/- 0.0110153
  row.laserDriftVelocityWest	 =   5.53526; // +/- 1.44459e-05 cm/us All: West = 0.258014 +/- 0.00273694
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53526 +/- 1.44459e-05
  return (TDataSet *)tableSet;// West = 5.53506 +/- 1.49758e-05 East = 5.53792 +/- 5.47893e-05
};
