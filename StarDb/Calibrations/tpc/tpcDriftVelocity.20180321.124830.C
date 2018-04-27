TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54946; // +/- 1.70878e-05 cm/us All: East = -1.01582 +/- 0.0144587
  row.laserDriftVelocityWest	 =   5.54946; // +/- 1.70878e-05 cm/us All: West = 0.232285 +/- 0.0032166
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54946 +/- 1.70878e-05
  return (TDataSet *)tableSet;// West = 5.54915 +/- 1.75069e-05 East = 5.55578 +/- 7.85653e-05
};
