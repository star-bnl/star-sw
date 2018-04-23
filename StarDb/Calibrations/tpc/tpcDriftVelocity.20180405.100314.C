TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52206; // +/- 4.27588e-05 cm/us All: East = -0.381286 +/- 0.0466056
  row.laserDriftVelocityWest	 =   5.52206; // +/- 4.27588e-05 cm/us All: West = 0.228069 +/- 0.0147164
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52206 +/- 4.27588e-05
  return (TDataSet *)tableSet;// West = 5.52134 +/- 4.76317e-05 East = 5.52507 +/- 9.70438e-05
};
