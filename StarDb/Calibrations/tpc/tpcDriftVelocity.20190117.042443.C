TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 16073
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55755; // +/- 1.02558e-05 cm/us All: East = -0.068077 +/- 0.0045174
  row.laserDriftVelocityWest	 =   5.55755; // +/- 1.02558e-05 cm/us All: West = 0.347152 +/- 0.00209541
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55755 +/- 1.02558e-05
  return (TDataSet *)tableSet;// West = 5.55713 +/- 1.12615e-05 East = 5.55962 +/- 2.48283e-05
};
